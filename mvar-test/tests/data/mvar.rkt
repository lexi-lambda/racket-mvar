#lang racket/base

(require data/mvar
         racket/contract
         rackunit)

(check-equal? (mvar-empty? (make-mvar)) #t)
(check-equal? (mvar-empty? (make-mvar 1)) #f)

(check-equal? (mvar-take! (make-mvar 1)) 1)
(check-equal? (mvar-try-take! (make-mvar 1)) 1)
(check-equal? (mvar-try-take! (make-mvar)) #f)
(check-equal? (mvar-try-take! (make-mvar) 'fail) 'fail)
(check-equal? (mvar-try-take! (make-mvar) (λ () 'fail)) 'fail)

(check-equal? (mvar-peek (make-mvar 1)) 1)
(check-equal? (mvar-try-peek (make-mvar 1)) 1)
(check-equal? (mvar-try-peek (make-mvar)) #f)
(check-equal? (mvar-try-peek (make-mvar) 'fail) 'fail)
(check-equal? (mvar-try-peek (make-mvar) (λ () 'fail)) 'fail)

(check-equal? (mvar-put! (make-mvar) 1) (void))
(check-equal? (mvar-try-put! (make-mvar) 1) #t)
(check-equal? (mvar-try-put! (make-mvar 1) 2) #f)

(test-case
 "mvar-peek completes before mvar-take!"
 (define mv1 (make-mvar))
 (define mv2 (make-mvar))
 (define mv3 (make-mvar))
 (for ([i (in-range 100)])
   (thread (λ () (mvar-put! mv2 (mvar-take! mv1)))))
 (thread (λ () (mvar-put! mv3 (mvar-peek mv1))))
 (sync (system-idle-evt)) ; ensure peeker is waiting
 (mvar-put! mv1 10)
 (check-equal? (mvar-peek mv2) 10)
 (check-equal? (mvar-peek mv3) 10))

(test-case
 "threads are garbage collected if blocked indefinitely"
 (define exec (make-will-executor))
 (let ()
   (define mv (make-mvar))
   (will-register exec
                  (thread (λ () (mvar-take! mv)))
                  (λ (t) 'dead))
   (will-register exec
                  (thread (λ () (mvar-peek mv)))
                  (λ (t) 'dead)))
 (let ()
   (define mv (make-mvar 1))
   (will-register exec
                  (thread (λ () (mvar-put! mv 2)))
                  (λ (t) 'dead)))

 (define (execute-one)
   (let loop ([i 0])
     (if (>= i 10)
         (error 'execute-one "thread still alive after 10 collections")
         (or (will-try-execute exec)
             (begin
               (collect-garbage (if (< i 2) 'minor 'major))
               (loop (add1 i)))))))

 (check-equal? (execute-one) 'dead)
 (check-equal? (execute-one) 'dead)
 (check-equal? (execute-one) 'dead))

(test-begin
 "basic mvar-swap! usage"
 (define mv (make-mvar 1))
 (check-equal? (mvar-swap! mv 2) 1)
 (check-equal? (mvar-swap! mv 3) 2)
 (check-equal? (mvar-swap! mv 4) 3)
 (check-equal? (mvar-try-peek mv) 4))

(test-begin
 "basic mvar-update! usage"
 (define mv (make-mvar 1))
 (check-equal? (mvar-try-peek mv) 1)
 (mvar-update! mv add1)
 (check-equal? (mvar-try-peek mv) 2)
 (mvar-update! mv add1)
 (check-equal? (mvar-try-peek mv) 3))

(test-case
 "mvar-update! restores the value on exceptions"
 (define mv (make-mvar 1))
 (check-exn exn:fail? (λ () (mvar-update! mv (λ () (error "bang")))))
 (check-equal? (mvar-try-peek mv) 1))

(test-case
 "basic call-with-mvar usage"
 (define mv (make-mvar 1))
 (check-equal? (call-with-mvar mv (λ (val) (add1 val))) 2)
 (check-equal? (mvar-try-peek mv) 1))

(test-case
 "multi-valued return from call-with-mvar"
 (define mv (make-mvar 1))
 (check-equal? (call-with-values (λ () (call-with-mvar mv (λ (val) (values)))) list)
               '())
 (check-equal? (call-with-values (λ () (call-with-mvar mv (λ (val) (values (add1 val) (sub1 val))))) list)
               '(2 0))
 (check-equal? (mvar-try-peek mv) 1))

(test-case
 "call-with-mvar! zero-valued return error message"
 (check-exn #px"expected: at least 1\n  received: 0"
            (λ () (call-with-mvar! (make-mvar 1) (λ (val) (values))))))

(define ((exn?-blaming which) exn)
  (and (exn:fail:contract:blame? exn)
       (let ([b (exn:fail:contract:blame-object exn)])
         (equal? (blame-positive b) which))))

(test-case
 "mvar/c blame"
 (define mv (make-mvar))
 (define mv+c (contract (mvar/c exact-integer?) mv 'pos 'neg))
 (check-equal? (mvar-put! mv+c 10) (void))
 (check-equal? (mvar-peek mv+c) 10)
 (check-equal? (mvar-take! mv+c) 10)
 (check-exn (exn?-blaming 'neg) (λ () (mvar-put! mv+c 'not-an-integer)))
 (check-equal? (mvar-empty? mv+c) #t)
 (mvar-put! mv 'not-an-integer)
 (check-exn (exn?-blaming 'pos) (λ () (mvar-peek mv+c)))
 (check-exn (exn?-blaming 'pos) (λ () (mvar-take! mv+c)))
 (check-equal? (mvar-empty? mv+c) #t))
