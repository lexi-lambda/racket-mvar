#lang racket/base

(require racket/contract
         "mvar/private/util/contract.rkt")

(provide (contract-out
          [mvar? predicate/c]
          [make-mvar (case-> (-> mvar?)
                             (-> any/c mvar?))]
          [mvar-put! (->* [mvar? any/c] [#:enable-break? any/c] void?)]
          [mvar-try-put! (-> mvar? any/c boolean?)]
          [mvar-put!-evt (-> mvar? any/c evt?)]
          [mvar-take! (->* [mvar?] [#:enable-break? any/c] any/c)]
          [mvar-try-take! (->* [mvar?] [failure-result/c] any)]
          [mvar-take!-evt (-> mvar? evt?)]
          [mvar-peek (->* [mvar?] [#:enable-break? any/c] any/c)]
          [mvar-try-peek (->* [mvar?] [failure-result/c] any)]
          [mvar-peek-evt (-> mvar? evt?)]
          [mvar-empty? (-> mvar? boolean?)]
          [mvar-empty-evt (-> mvar? evt?)]

          [mvar-swap! (->* [mvar? any/c] [#:enable-break? any/c] any/c)]
          [mvar-update! (->* [mvar? (-> any/c any/c)] [#:enable-break? any/c] void?)]
          [call-with-mvar (->* [mvar? (-> any/c any)] [#:enable-break? any/c] any)]
          [call-with-mvar! (->* [mvar? (-> any/c any)] [#:enable-break? any/c] any)]

          [impersonate-mvar impersonate-mvar/c]
          [chaperone-mvar impersonate-mvar/c]
          [mvar/c (->* [contract?] [contract?] contract?)]))

(define no-value (gensym 'no-value))

;; -----------------------------------------------------------------------------
;; core operations

;; An mvar is implemented as a mutable cell combined with two semaphores and a
;; channel. The semaphores are waited on by threads trying to take or put, and
;; the channel is waited on by threads trying to peek.
;;
;; Ordinary takes and puts are fairly straightforward. When a thread wants to
;; take a value out of the mvar, it decrements the take semaphore, swaps the
;; value in the cell with #f, and increments the put semaphore. Likewise, when
;; a thread wants to put a value into the mvar, it decrements the put semaphore,
;; mutates the cell, and increments the take semaphore. This means that the
;; semaphore counters are always either 0 or 1, and they are never both 1 at the
;; same time (but are briefly zero while an exchange is occurring).
;;
;; “Peeking” could be implemented by simply taking the value and immediately
;; putting it back, but this creates the possibility for a different value to be
;; put into the mvar before the peeked value can be returned. Therefore, we
;; implement a separate peeking operation as a primitive, as in Haskell. In
;; addition to preventing the aforementioned scenario, all threads blocked on a
;; peek operation are guaranteed to be woken up when a value is put into the
;; mvar, before any other thread has a chance to take the value out again. This
;; makes peeking useful in single writer, multiple readers scenarios, since all
;; peeking threads are woken up at the same time.
;;
;; When a thread peeks at an mvar, it starts by polling the take semaphore in
;; order to check if a value is already there, but if it isn’t, it blocks on a
;; channel. When a thread puts a value into an mvar, it also puts the value into
;; the channel until there aren’t any more threads waiting on it before it
;; increments the take semaphore.
(struct mvar ([value #:mutable] take-sem put-sem peek-chan)
  #:property prop:custom-write
  (λ (self out mode)
    (define v (mvar-try-peek self no-value))
    (if (eq? v no-value)
        (write-string "#<mvar: empty>" out)
        (fprintf out "#<mvar: ~v>" v))))

(define make-mvar
  (case-lambda
    [()
     (mvar #f
           (make-semaphore)
           (make-semaphore 1)
           (make-channel))]
    [(v)
     (define mv (make-mvar))
     (mvar-put! mv v)
     mv]))

(define (do-put mv v)
  (set-mvar-value! mv v)
  (let loop ()
    (sync/timeout 0 (handle-evt (channel-put-evt (mvar-peek-chan mv) v)
                                (λ (evt) (loop)))))
  (semaphore-post (mvar-take-sem mv)))

(define (mvar-put! mv v #:enable-break? [enable-break? #f])
  (define v* (apply-mvar-put-proj mv v))
  (define wait (if (or enable-break? (break-enabled))
                   semaphore-wait/enable-break
                   semaphore-wait))
  (parameterize-break #f
    (wait (mvar-put-sem mv))
    (do-put mv v*)))

(define (mvar-try-put! mv v)
  (define v* (apply-mvar-put-proj mv v))
  (parameterize-break #f
    (if (semaphore-try-wait? (mvar-put-sem mv))
        (begin
          (do-put mv v*)
          #t)
        #f)))

(define (mvar-put!-evt mv v)
  (define v* (apply-mvar-put-proj mv v))
  (define evt (wrap-evt (mvar-put-sem mv)
                        (λ (sem)
                          (do-put mv v*)
                          evt)))
  evt)

(define (do-take mv)
  (define v (mvar-value mv))
  (set-mvar-value! mv #f)
  (semaphore-post (mvar-put-sem mv))
  v)

(define (mvar-take! mv #:enable-break? [enable-break? #f])
  (define wait (if (or enable-break? (break-enabled))
                   semaphore-wait/enable-break
                   semaphore-wait))
  (apply-mvar-get-proj
   mv
   (parameterize-break #f
     (wait (mvar-take-sem mv))
     (do-take mv))))

(define (mvar-try-take! mv [fail #f])
  (define v (parameterize-break #f
              (if (semaphore-try-wait? (mvar-take-sem mv))
                  (do-take mv)
                  no-value)))
  (if (eq? v no-value)
      (get-failure-result fail)
      (apply-mvar-get-proj mv v)))

(define (mvar-take!-evt mv)
  (handle-evt (wrap-evt (mvar-take-sem mv)
                        (λ (sem) (do-take mv)))
              (λ (v) (apply-mvar-get-proj mv v))))

(define (do-peek mv)
  (define v (mvar-value mv))
  (semaphore-post (mvar-take-sem mv))
  v)

(define (mvar-peek-evt mv)
  (handle-evt (choice-evt (mvar-peek-chan mv)
                          (wrap-evt (mvar-take-sem mv) (λ (sem) (do-peek mv))))
              (λ (v) (apply-mvar-get-proj mv v))))

(define (mvar-peek mv #:enable-break? [enable-break? #f])
  ((if enable-break? sync/enable-break sync)
   (mvar-peek-evt mv)))

(define (mvar-try-peek mv [fail #f])
  (define v (parameterize-break #f
              (if (semaphore-try-wait? (mvar-take-sem mv))
                  (do-peek mv)
                  no-value)))
  (if (eq? v no-value)
      (get-failure-result fail)
      (apply-mvar-get-proj mv v)))

(define (mvar-empty? mv)
  (eq? (mvar-try-peek mv no-value) no-value))

;; The ability to block until an mvar is empty, rather than simply block on
;; putting a value into the mvar, may seem somewhat unusual. Indeed, Haskell
;; does not provide any such operation. But in fact it can be quite useful in
;; combination with Racket’s synchronizable events system, as it can be used as
;; as signal in an event loop that the mvar is ready to be refilled, and the
;; event loop can spend its time working on other things until that happens.
(define (mvar-empty-evt mv)
  (semaphore-peek-evt (mvar-put-sem mv)))

(define (get-failure-result fail)
  (if (procedure? fail)
      (fail)
      fail))

;; -----------------------------------------------------------------------------
;; derived operations

(define (mvar-swap! mv new-val #:enable-break? [enable-break? #f])
  (define breaks? (break-enabled))
  (parameterize-break #f
    (define old-val (mvar-take! mv #:enable-break? (or breaks? enable-break?)))
    (mvar-put! mv new-val)
    old-val))

(define (call-with-mvar! mv proc #:enable-break? [enable-break? #f])
  (define break-paramz (current-break-parameterization))
  (define breaks? (break-enabled))
  (parameterize-break #f
    (define old-val (mvar-take! mv #:enable-break? (or breaks? enable-break?)))
    (define new-val old-val)
    (dynamic-wind
     void
     (λ ()
       (call-with-continuation-barrier
        (λ ()
          (call-with-break-parameterization
           break-paramz
           (λ ()
             (call-with-values
              (λ () (proc old-val))
              (case-lambda
                [() (raise-arguments-error
                     'call-with-mvar!
                     "contract violation;\n given procedure returned wrong number of results"
                     "expected" (unquoted-printing-string "at least 1")
                     "received" 0
                     "procedure" proc)]
                [(val)
                 (set! new-val val)
                 (values)]
                [(val result)
                 (set! new-val val)
                 (values result)]
                [(val result1 result2)
                 (set! new-val val)
                 (values result1 result2)]
                [(val . results)
                 (set! new-val val)
                 (apply values results)])))))))
     (λ () (mvar-put! mv new-val)))))

(define (mvar-update! mv proc #:enable-break? [enable-break? #f])
  (call-with-mvar!
   mv #:enable-break? enable-break?
   (λ (val) (values (proc val) (void)))))

(define (call-with-mvar mv proc #:enable-break? [enable-break? #f])
  (call-with-mvar!
   mv #:enable-break? enable-break?
   (λ (val)
     (call-with-values
      (λ () (proc val))
      (case-lambda
        [() val]
        [(result) (values val result)]
        [(result1 result2) (values val result1 result2)]
        [results (apply values val results)])))))

;; -----------------------------------------------------------------------------
;; chaperones and impersonators

;; To implement chaperones and impersonators for mvars, get and put
;; projections are attached to mvar values as impersonator properties.
;; Primitive operations on mvars extract and apply the attached
;; projections as appropriate using `apply-mvar-put-proj` and/or
;; `apply-mvar-get-proj`.

(define-values [impersonator-prop:mvar-get-proj
                has-mvar-get-proj?
                get-mvar-get-proj]
  (make-impersonator-property 'mvar-get-proj))
(define-values [impersonator-prop:mvar-put-proj
                has-mvar-put-proj?
                get-mvar-put-proj]
  (make-impersonator-property 'mvar-put-proj))

(define (apply-mvar-get-proj mv v)
  (define get-proj (get-mvar-get-proj mv #f))
  (if get-proj
      (get-proj v)
      v))
(define (apply-mvar-put-proj mv v)
  (define put-proj (get-mvar-put-proj mv #f))
  (if put-proj
      (put-proj v)
      v))

(define (wrap-mvar wrap-proc mv props #:get get-proc #:put put-proc)
  (let* ([props (if get-proc
                    (list* impersonator-prop:mvar-get-proj
                           (let ([prev-get-proc (get-mvar-get-proj mv #f)])
                             (if prev-get-proc
                                 (λ (v) (get-proc (prev-get-proc v)))
                                 get-proc))
                           props)
                    props)]
         [props (if put-proc
                    (list* impersonator-prop:mvar-put-proj
                           (let ([prev-put-proc (get-mvar-put-proj mv #f)])
                             (if prev-put-proc
                                 (λ (v) (prev-put-proc (put-proc v)))
                                 put-proc))
                           props)
                    props)])
    (apply wrap-proc mv struct:mvar props)))

(define (impersonate-mvar mv #:get [get-proc #f] #:put [put-proc #f] . props)
  (wrap-mvar impersonate-struct mv get-proc put-proc props))
(define (chaperone-mvar mv #:get [get-proc #f] #:put [put-proc #f] . props)
  (define ((make-checked-chaperone-proc proc) a)
    (define b (proc a))
    (unless (chaperone-of? b a)
      (raise-arguments-error '|mvar chaperone|
                             "non-chaperone result;\n received a value that is not a chaperone of the original value"
                             "value" a
                             "non-chaperone value" b))
    b)
  (wrap-mvar chaperone-struct
             mv
             props
             #:get (and get-proc (make-checked-chaperone-proc get-proc))
             #:put (and put-proc (make-checked-chaperone-proc put-proc))))

(define impersonate-mvar/c
  (->* [mvar?]
       [#:get (or/c (-> any/c any/c) #f)
        #:put (or/c (-> any/c any/c) #f)]
       #:rest impersonator-properties/c
       mvar?))

;; -----------------------------------------------------------------------------
;; mvar/c

(define (make-mvar-contract-property build-contract-property wrap-mvar)
  (define ((lift-relation <=?) a b)
    (and (mvar-contract? b)
         (<=? (mvar-contract-put-ctc a)
              (mvar-contract-put-ctc b))
         (or (and (not (mvar-contract-get-ctc a))
                  (not (mvar-contract-get-ctc b)))
             (<=? (or (mvar-contract-put-ctc a)
                      (mvar-contract-get-ctc a))
                  (or (mvar-contract-put-ctc b)
                      (mvar-contract-get-ctc b))))))

  (build-contract-property
   #:name
   (λ (self)
     (define put-ctc (mvar-contract-put-ctc self))
     (define get-ctc (mvar-contract-get-ctc self))
     (if get-ctc
         (build-compound-type-name 'mvar/c put-ctc get-ctc)
         (build-compound-type-name 'mvar/c put-ctc)))
   #:first-order (λ (self) mvar?)
   #:stronger (lift-relation contract-stronger?)
   #:equivalent (lift-relation contract-equivalent?)
   #:late-neg-projection
   (λ (self)
     (define put-proc (get/build-late-neg-projection (mvar-contract-put-ctc self)))
     (define get-ctc (mvar-contract-get-ctc self))
     (define get-proc (if get-ctc (get/build-late-neg-projection get-ctc) put-proc))
     (λ (blame)
       (build-pos/neg-val-projection
        (λ () (get-proc (blame-add-context blame "a value read from")))
        (λ () (put-proc (blame-add-context blame "a value written to" #:swap? #t)))
        (λ (get-proj put-proj)
          (λ (val neg-party)
            (unless (mvar? val)
              (raise-blame-error
               blame val #:missing-party neg-party
               (list 'expected: "mvar?" 'given: "~e") val))

            (define blame+neg-party (cons blame neg-party))
            (wrap-mvar
             val
             #:get (λ (val)
                     (with-contract-continuation-mark blame+neg-party
                       (get-proj val neg-party)))
             #:put (λ (val)
                     (with-contract-continuation-mark blame+neg-party
                       (put-proj val neg-party)))
             impersonator-prop:contracted self
             impersonator-prop:blame blame+neg-party))))))))

(struct mvar-contract (put-ctc get-ctc)
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write contract-custom-write-property-proc)
(struct impersonator-mvar-contract mvar-contract ()
  #:property prop:contract
  (make-mvar-contract-property build-contract-property impersonate-mvar))
(struct chaperone-mvar-contract mvar-contract ()
  #:property prop:chaperone-contract
  (make-mvar-contract-property build-chaperone-contract-property chaperone-mvar))

(define (mvar/c put-ctcish [get-ctcish no-value])
  (define put-ctc (coerce-contract 'mvar/c put-ctcish))
  (define get-ctc (if (eq? get-ctcish no-value)
                      #f
                      (coerce-contract 'mvar/c get-ctcish)))
  (if (and (chaperone-contract? put-ctc)
           (or (not get-ctc) (chaperone-contract? get-ctc)))
      (chaperone-mvar-contract put-ctc get-ctc)
      (impersonator-mvar-contract put-ctc get-ctc)))
