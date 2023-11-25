#lang scribble/manual

@(begin
   (require (for-label data/mvar
                       racket/base
                       racket/contract)
            scribble/example
            scriblib/autobib)

   (define (reftech . pre-content)
     (apply tech pre-content #:doc '(lib "scribblings/reference/reference.scrbl")))

   (define-cite ~cite citet generate-bibliography)
   (define concurrent-haskell
     (make-bib
      #:title "Concurrent Haskell"
      #:author (authors (author-name "Simon" "Peyton Jones") "Andrew Gordon" "Sigbjorn Finne")
      #:location "Principles of Programming Languages (POPL)"
      #:date 1996
      #:doi "10.1145/237721.237794"))

   (define make-mvar-eval (make-eval-factory '(data/mvar racket/contract)))
   (define-syntax-rule (mvar-examples body ...)
     (examples #:eval (make-mvar-eval) #:once body ...)))

@title{M-vars: Synchronized Boxes}
@author{@author+email["Alexis King" "lexi.lambda@gmail.com"]}
@margin-note{The source of this manual is available on @hyperlink["https://github.com/lexi-lambda/racket-mvar/blob/master/mvar-doc/scribblings/mvar.scrbl"]{GitHub.}}

@defmodule[data/mvar]{

@margin-note{M-vars originate in Haskell, where they are known as @code{MVar}s. This library is based on the @hyperlink["https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Concurrent-MVar.html"]{modern API provided by GHC}, which differs in some details from their original presentation in @citet[concurrent-haskell]. Most notably, @racket[mvar-put!] on a full M-var blocks (instead of raising an exception), and @racket[mvar-peek] is atomic.}
                 
An @deftech{M-var} is a mutable data structure useful in concurrent programs. Like a @reftech{box}, an M-var is a mutable reference cell that can hold a single value. Unlike a box, an M-var can also be @deftech{empty}, holding no value at all. When a value is placed into an empty M-var using @racket[mvar-put!], the M-var becomes @deftech{full}, and it remains full until the value is removed using @racket[mvar-take!]. If a thread attempts to put a value into an M-var that is already full, the thread waits until the M-var is emptied. Dually, if a thread attempts to take a value from an M-var that is currently empty, it waits until the M-var is filled.

It is also possible to atomically read the contents of a full M-var without removing its value using @racket[mvar-peek]. Like @racket[mvar-take!], using @racket[mvar-peek] on an empty M-var waits until it is filled. Each operation also comes in a polling variant: @racket[mvar-try-put!], @racket[mvar-try-take!], and @racket[mvar-try-peek] always return immediately and simply fail instead of blocking. For maximum flexibility, M-vars can also be combined with other @reftech{synchronizable events} using @racket[mvar-put!-evt], @racket[mvar-take!-evt], and @racket[mvar-peek-evt].

The blocking behavior of M-var operations makes it a remarkably flexible building block in concurrent programs, as it is effectively a combination of a @reftech{box}, a @reftech{semaphore}, and a @reftech{channel}. Even just a single M-var can be used in many different ways:
@;
@itemlist[
 @item{If separate processes are tasked with filling and emptying an M-var, it behaves like an @reftech{asynchronous channel} with a buffer size of 1. Producers use @racket[mvar-put!] to send a value, and consumers use @racket[mvar-take!] to receive a value.}

 @item{If an M-var is normally kept full, it behaves like a @reftech{box} protected by a @reftech{semaphore}. Readers use @racket[mvar-peek] and do not block each other. Writers use @racket[mvar-take!] to acquire the lock and @racket[mvar-put!] to both update the value and release the lock.}

 @item{If an M-var is normally kept empty, it behaves like a @hyperlink["https://en.wikipedia.org/wiki/Monitor_(synchronization)#Nonblocking_condition_variables"]{nonblocking, broadcast condition variable}. @racket[mvar-peek] is used to wait on the condition, and @racket[mvar-put!] followed immediately by @racket[mvar-take!] is used to notify waiters.}

 @item{If an M-var starts empty, is filled exactly once, and subsequently remains full, @racket[mvar-peek-evt] can be used to obtain a @reftech{synchronizable event} that remains permanently @reftech{ready for synchronization} once it has been signaled.}]
@;
This list is far from exhaustive, and multiple M-vars used in concert can be even more flexible.}

@section[#:tag "ordering-and-fairness"]{Ordering and Fairness}

@tech{M-var} synchronization is @deftech{fair}: if a thread is blocked on an M-var operation, and opportunities for the operation to complete occur infinitely often, the operation is guaranteed to eventually complete.

Additionally, if a thread is blocked on a call to @racket[mvar-peek], the call is guaranteed to complete the next time the M-var is @tech[#:key "full"]{filled}, even if another thread is blocked on a call to @racket[mvar-take!] on the same M-var. In other words, whenever @racket[mvar-peek] and @racket[mvar-take!] compete to read the next value of an @tech{empty} M-var, @racket[mvar-peek] always “wins”. Since @racket[mvar-peek] is not exclusive—that is, it does not preclude another thread from reading the same M-var after it completes—this preference for @racket[mvar-peek] ensures that the maximum number of threads are woken up each time an M-var is filled.

@section[#:tag "core-operations"]{Core Operations}

@defproc*[([(make-mvar) mvar?]
           [(make-mvar [v any/c]) mvar?])]{
Creates and returns a new @tech{M-var}. If called with no arguments, the returned M-var is initially @tech{empty}. If called with one argument, the returned M-var is initially @tech{full} and contains @racket[v].

@(mvar-examples
  (make-mvar)
  (make-mvar 42))}

@defproc[(mvar? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is an @tech{M-var}, otherwise returns @racket[#f].}

@defproc[(mvar-put! [mv mvar?] [v any/c] [#:enable-break? enable-break? any/c #f]) void?]{
Fills @racket[mv] with the value @racket[v]. If @racket[mv] is already @tech{full}, @racket[mvar-put!] blocks until it is @tech[#:key "empty"]{emptied}.

If @racket[enable-break?] is not @racket[#f], @reftech{breaks} are explicitly enabled while waiting on @racket[mv]. If breaks are disabled when @racket[mvar-put!] is called, then either @racket[mv] is filled or an @racket[exn:break] exception is raised, but not both.

@(mvar-examples
  (define mv (make-mvar))
  mv
  (mvar-put! mv 42)
  mv)}

@defproc[(mvar-try-put! [mv mvar?] [v any/c]) boolean?]{
If @racket[mv] is currently @tech{empty}, @racket[mvar-try-put!] @tech[#:key "full"]{fills} it and returns @racket[#t]. Otherwise, returns @racket[#f].

@(mvar-examples
  (define mv (make-mvar))
  mv
  (eval:check (mvar-try-put! mv 1) #t)
  mv
  (eval:check (mvar-try-put! mv 2) #f)
  mv)}

@defproc[(mvar-put!-evt [mv mvar?] [v any/c]) evt?]{
Returns a @reftech{synchronizable event} for use with @racket[sync]. The event is @reftech{ready for synchronization} when @racket[mv] is @tech{empty}, and if the event is selected, @racket[mv] is @tech[#:key "full"]{filled} with @racket[v]. The event’s @reftech{synchronization result} is the event itself.}

@defproc[(mvar-take! [mv mvar?] [#:enable-break? enable-break? any/c #f]) any/c]{
Removes the value contained in @racket[mv] and returns it. If @racket[mv] is currently @tech{empty}, @racket[mvar-take!] blocks until it is @tech[#:key "full"]{filled}.

If @racket[enable-break?] is not @racket[#f], @reftech{breaks} are explicitly enabled while waiting on @racket[mv]. If breaks are disabled when @racket[mvar-take!] is called, then either @racket[mv] is emptied or an @racket[exn:break] exception is raised, but not both.

@(mvar-examples
  (define mv (make-mvar 42))
  mv
  (eval:check (mvar-take! mv) 42)
  mv)}

@defproc[(mvar-try-take! [mv mvar?] [fail failure-result/c #f]) any]{
If @racket[mv] is currently @tech{full}, @racket[mvar-try-take!] removes its value and returns it. If @racket[mv] is currently @tech{empty}, @racket[fail] determines the result:
@itemlist[
 @item{If @racket[fail] is a procedure, it is applied to zero arguments in tail position to produce the result.}
 @item{Otherwise, @racket[fail] is returned as the result.}]

@(mvar-examples
  (define mv (make-mvar 42))
  mv
  (eval:check (mvar-try-take! mv) 42)
  mv
  (eval:check (mvar-try-take! mv) #f))}

@defproc[(mvar-take!-evt [mv mvar?]) evt?]{
Returns a @reftech{synchronizable event} for use with @racket[sync]. The event is @reftech{ready for synchronization} when @racket[mv] is @tech{full}. If the event is selected, @racket[mv] is @tech[#:key "empty"]{emptied}, and the removed value is the event’s @reftech{synchronization result}.}

@defproc[(mvar-peek [mv mvar?] [#:enable-break? enable-break? any/c #f]) any/c]{
Returns the value contained in @racket[mv]. If @racket[mv] is currently @tech{empty}, @racket[mvar-peek] blocks until it is @tech[#:key "full"]{filled}.

If @racket[enable-break?] is not @racket[#f], @reftech{breaks} are explicitly enabled while waiting on @racket[mv]. If breaks are disabled when @racket[mvar-peek] is called, then either @racket[mv] is emptied or an @racket[exn:break] exception is raised, but not both.

@(mvar-examples
  (define mv (make-mvar 42))
  (eval:check (mvar-peek mv) 42)
  mv
  (eval:check (mvar-peek mv) 42))

Note that @racket[mvar-take!] followed immediately by a use of @racket[mvar-put!] to replace the taken value is @emph{not} equivalent to @racket[mvar-peek]: since @racket[mvar-take!] @tech[#:key "empty"]{empties} the M-var, another thread may @tech[#:key "full"]{fill} it with a different value before the removed value can be replaced. In comparison, @racket[mvar-peek] does not remove the value from the M-var, so it is guaranteed to be atomic. Additionally, a call to @racket[mvar-peek] is guaranteed to return as soon as the M-var is filled, while @racket[mvar-take!] is not; see @secref["ordering-and-fairness"].}

@defproc[(mvar-try-peek [mv mvar?] [fail failure-result/c #f]) any]{
If @racket[mv] is currently @tech{full}, @racket[mvar-try-peek] returns its value. If @racket[mv] is currently @tech{empty}, @racket[fail] determines the result in the same was as for @racket[mvar-try-take!].

@(mvar-examples
  (define mv (make-mvar 42))
  (eval:check (mvar-try-peek mv) 42)
  (eval:check (mvar-take! mv) 42)
  (eval:check (mvar-try-peek mv) #f))}

@defproc[(mvar-peek-evt [mv mvar?]) evt?]{
Returns a @reftech{synchronizable event} for use with @racket[sync]. The event is @reftech{ready for synchronization} when @racket[mv] is @tech{full}, and its value is the event’s @reftech{synchronization result}.}

@defproc[(mvar-empty? [mv mvar?]) boolean?]{
Returns @racket[#t] if @racket[mv] is currently @tech{empty}, otherwise returns @racket[#f].

This operation is provided for completeness, but note that if @racket[mv] has multiple readers, the result of this function could become out of date the moment it returns. It is therefore very rarely the right choice, and it is almost always better to use @racket[mvar-try-put!], @racket[mvar-try-take!], or @racket[mvar-try-peek], instead.}

@defproc[(mvar-empty-evt [mv mvar?]) evt?]{
Returns a @reftech{synchronizable event} for use with @racket[sync]. The event is @reftech{ready for synchronization} when @racket[mv] is @tech{empty}, and its @reftech{synchronization result} is the event itself.

Like @racket[mvar-empty?], this operation should be used very carefully: even if the event is selected, another thread might fill @racket[mv] the instant that @racket[sync] returns, so it is almost always better to use @racket[mvar-put!-evt], instead. However, in programs where @racket[mv] only has a single writer, it can rarely be useful, so it is provided for completeness.}

@section{Contracts}

@defproc[(mvar/c [in-ctc contract?] [out-ctc contract? in-ctc]) contract?]{
Returns a @reftech{contract} that recognizes @tech{M-vars}. Values written to the M-var must match @racket[in-ctc], and values read from the M-var must match @racket[out-ctc]. Usually, @racket[in-ctc] and @racket[out-ctc] are the same (which is the default if @racket[out-ctc] is not provided), but supplying @racket[none/c] for one of the arguments can be useful to restrict the client of the contract to reading from or writing to the M-var.

If @racket[in-ctc] and @racket[out-ctc] are both @reftech{chaperone contracts}, the result will be a chaperone contract. Otherwise, the result will be an @reftech{impersonator contract}.

@(mvar-examples
  (define/contract mv (mvar/c exact-integer?) (make-mvar))
  (eval:error (mvar-put! mv 'not-an-integer)))}

@section{Chaperones and Impersonators}

@defproc[(impersonate-mvar [mv mvar?]
                           [#:get get-proc (or/c (-> any/c any/c) #f) #f]
                           [#:put put-proc (or/c (-> any/c any/c) #f) #f]
                           [prop impersonator-property?]
                           [prop-val any/c]
                           ... ...)
         mvar?]{
Returns an @reftech{impersonator} of @racket[mv].

If @racket[get-proc] is not @racket[#f], the result of each use of @racket[mvar-take!] or @racket[mvar-peek] on the impersonator is redirected through @racket[get-proc], which must produce a replacement value. Likewise, if @racket[put-proc] is not @racket[#f], the value stored by each use of @racket[mvar-put!] is redirected through @racket[put-proc].

Pairs of @racket[prop] and @racket[prop-val] (the number of by-position arguments to @racket[impersonate-mvar] must be odd) add or override @reftech{impersonator property} values of @racket[mv].}

@defproc[(chaperone-mvar [mv mvar?]
                         [#:get get-proc (or/c (-> any/c any/c) #f) #f]
                         [#:put put-proc (or/c (-> any/c any/c) #f) #f]
                         [prop impersonator-property?]
                         [prop-val any/c]
                         ... ...)
         mvar?]{
Like @racket[impersonate-mvar], but produces a @reftech{chaperone} of @racket[mv], and the @racket[get-proc] and @racket[put-proc] procedures must return chaperones of their arguments.}

@generate-bibliography[]
