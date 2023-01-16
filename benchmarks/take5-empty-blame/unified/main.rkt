(module main racket/base
  (#%module-begin
   (provide main)
   (require require-typed-check racket/class "../base/untyped.rkt" "type-interface.rkt")
   (require (only-in "player.rkt" create-player))
   (define (main n)
     (define k
       (cond
        ((and (string? n) (string->number n)) => (lambda (x) (assert n index?)))
        ((index? n) n)
        (else (error 'main "input must be a natural number; given ~e" n))))
     (define players (build-list k create-player))
     (define dealer (create-dealer players))
     (send dealer play-game))
   (define PLAYERS 10)
   (define LOOPS 1)
   (module+
    test
    (unless (equal?
             (main PLAYERS)
             '((after-round 2)
               ((1 0) (2 0) (3 0) (6 0) (7 0) (8 0) (0 56) (4 80) (9 80) (5 120))))
      (raise-user-error 'take5 "TEST FAILURE")))
   (module+ main (time (for ((n (in-range LOOPS))) (main PLAYERS))))))
