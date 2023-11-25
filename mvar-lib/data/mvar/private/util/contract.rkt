#lang racket/base

(require racket/contract
         racket/match)

(provide build-pos/neg-val-projection
         impersonator-properties/c)

;; Wraps `contract-pos/neg-doubling` in an API that doesn’t make me want to die.
(define (build-pos/neg-val-projection pos-thunk neg-thunk make-val-proj)
  (define-values [filled? maybe-pos maybe-neg]
    (contract-pos/neg-doubling (pos-thunk) (neg-thunk)))
  (cond
    [filled?
     (make-val-proj maybe-pos maybe-neg)]
    [else
     (define val-proj
       (λ (val neg-party)
         (define proj (make-val-proj (maybe-pos) (maybe-neg)))
         (set! val-proj proj)
         (proj val neg-party)))
     (λ (val neg-party)
       (val-proj val neg-party))]))

;; Like (flat-rec-contract impersonator-properties/c
;;        '() (cons/c impersonator-property? (cons/c any/c impersonator-properties/c)))
;; but provides better error messages.
(define impersonator-properties/c
  (let ()
    (define key-proc (get/build-late-neg-projection (coerce-contract 'impersonator-properties/c impersonator-property?)))
    (make-flat-contract
     #:list-contract? #t
     #:name '(pairsof impersonator-property? any/c)
     #:first-order
     (λ (v)
       (and (list? v)
            (let loop ([vs v])
              (match vs
                ['() #t]
                [(list* k _ vs)
                 (and (impersonator-property? k) (loop vs))]
                [_ #f]))))
     #:late-neg-projection
     (λ (blame)
       (λ (val neg-party)
         (unless (list? val)
           (raise-blame-error
            blame val #:missing-party neg-party
            '(expected: "list?" given: "~e") val))

         (define (check-prop k)
           (unless (impersonator-property? k)
             (raise-blame-error
              blame k #:missing-party neg-party
              '(expected: "impersonator-property?" given: "~e") k)))

         (let loop ([vs val])
           (match vs
             ['() (void)]
             [(list k)
              (raise-blame-error
               blame val #:missing-party neg-party
               '("impersonator property does not have a value (i.e. an odd number of elements were given)\n"
                 "  impersonator property: ~e")
               k)]
             [(list* k v vs)
              (unless (impersonator-property? k)
                (raise-blame-error
                 (blame-add-context blame "an element of")
                 k #:missing-party neg-party
                 '(expected: "impersonator-property?" given: "~e") k))
              (loop vs)]))
         val)))))
