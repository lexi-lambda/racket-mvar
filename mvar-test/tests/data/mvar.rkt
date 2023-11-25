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

(define ((exn?-blaming which) exn)
  (and (exn:fail:contract:blame? exn)
       (let ([b (exn:fail:contract:blame-object exn)])
         (equal? (blame-positive b) which))))

(test-begin
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
