(module player racket
  (#%module-begin
   (provide random-players ordered-players inf-loop-player)
   (require "../base/untyped.rkt" "type-interface.rkt")
   (require (only-in "strategy.rkt" ordered-s random-s))
   (define (create n c) (new player% (name n) (choice c)))
   (define player%
     (class object%
       (init-field name choice)
       (super-new)
       (field (*players '()) (*bad '()))
       (define *my-game-name "")
       (define/public (go administrator) (set! *my-game-name (send administrator sign-up name this)))
       (define/public (setup s) (bad-players (state-players s)))
       (define/public (take-turn turn) (bad-players (get-field players turn)) (choice (reconcile turn)))
       (define/public (keep hotels) (map (lambda (h) (< (random 100) 50)) hotels))
       (define/public (receive-tile t) (void))
       (define/public (inform s) (bad-players (state-players s)))
       (define/public (the-end state results) (void))
       (define/private
        (bad-players players)
        (set! *bad
          (for/fold
           ((bad *bad))
           ((old-player (in-list *players)))
           (define n (player-name old-player))
           (if (findf (lambda (current) (string=? (player-name current) n)) players) bad (cons old-player bad))))
        (set! *players players))
       (define/private (reconcile turn) (define bad-shares (*combine-shares (map player-shares *bad))) (send turn reconcile-shares bad-shares) turn)))
   (define (players S n prefix) (for/list ((name '("a" "b" "c" "d" "e" "f")) (i (in-range n))) (create (~a prefix '- name) S)))
   (define (random-players n) (players random-s n "random"))
   (define (ordered-players n) (players ordered-s n "ordered"))
   (define (inf-loop-player n) (define m 0) (define (S t) (if (> n m) (begin (set! m (+ m 1)) (ordered-s t)) (let L () (L)))) (create (format "inf loop after ~a" n) S))))
