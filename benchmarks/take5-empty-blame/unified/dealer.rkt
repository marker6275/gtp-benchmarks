(module dealer racket/base
  (#%module-begin
   (provide create-dealer)
   (require racket/list require-typed-check racket/class)
   (require "card.rkt")
   (struct card (face bulls) #:prefab)
   (require (only-in
             "basics.rkt"
             FACE
             FIVE
             STACKS
             SIXTYSIX
             HAND
             MIN-BULL
             MAX-BULL
             configuration))
   (require (only-in "card-pool.rkt" create-card-pool))
   (require (only-in "deck.rkt" create-deck))
   (require (only-in "base-player.rkt" player%))
   (define (default-order loc) (sort loc > #:key card-face))
   (define (default-faces) MIN-BULL)
   (define (create-dealer players) (new dealer% (players players)))
   (define dealer%
     (class object%
       (init-field players)
       (super-new)
       (field
        (internal%
         (class object%
           (super-new)
           (init-field player)
           (field (n (send player name)) (order default-order) (my-cards empty) (my-bulls 0))
           (define/public (name) n)
           (define/public (start-round cs) (send player start-round cs))
           (define/public (start-turn d) (send player start-turn d))
           (define/public (choose d) (send player choose d))
           (define/public (bulls) (send this get-field:my-bulls))
           (define/public
            (add-score n)
            (send this set-field:my-bulls (+ n (send this get-field:my-bulls))))
           (define/public (get-field:n) (send player get-field:n))
           (define/public (set-field:n v) (send player set-field:n v))
           (define/public (get-field:order) (send player get-field:order))
           (define/public (set-field:order v) (send player set-field:order v))
           (define/public (get-field:my-cards) (send player get-field:my-cards))
           (define/public (set-field:my-cards v) (send player set-field:my-cards v))
           (define/public (get-field:player) player)
           (define/public (get-field:my-bulls) my-bulls)
           (define/public (set-field:player v) (set! player v))
           (define/public (set-field:my-bulls v) (set! my-bulls v))))
        (internals
         (for/list ((p (in-list (send this get-field:players)))) (new internal% (player p)))))
       (define/public (get-field:players) players)
       (define/public (get-field:internal%) internal%)
       (define/public (get-field:internals) internals)
       (define/public (set-field:players v) (set! players v))
       (define/public (set-field:internal% v) (set! internal% v))
       (define/public (set-field:internals v) (set! internals v))
       (define/public
        (play-game (shuffle values) (faces default-faces))
        (define n (length (send this get-field:internals)))
        (when (> (+ (* n HAND) STACKS) FACE)
          (error 'play-game "cannot play with ~a players; more cards needed" n))
        (let play-game ((i 1))
          (play-round shuffle faces)
          (if (any-player-done?) (present-results i) (play-game (+ i 1)))))
       (define/public
        (present-results i)
        (define sorted
          (sort (send this get-field:internals) < #:key (lambda (i) (send i bulls))))
        `((after-round ,i)
          ,(for/list ((p (in-list sorted))) `(,(send p name) ,(send p bulls)))))
       (define/public
        (any-player-done?)
        (for/or ((p (in-list (send this get-field:internals)))) (> (send p bulls) SIXTYSIX)))
       (define/public
        (play-round shuffle faces)
        (define card-pool (create-card-pool shuffle faces))
        (define deck (create-deck card-pool))
        (deal-cards card-pool)
        (for ((p HAND)) (play-turn deck)))
       (define/private
        (deal-cards card-pool)
        (for
         ((p (in-list (send this get-field:internals))))
         (send p start-round (send card-pool draw-hand))))
       (define/private
        (play-turn deck)
        (define played-cards
          (for/list
           ((p (in-list (send this get-field:internals))))
           (list p (send p start-turn deck))))
        (define sorted-played-cards
          (sort played-cards < #:key (lambda (x) (card-face (second x)))))
        (place-cards deck sorted-played-cards))
       (define/private
        (place-cards deck sorted-player-cards)
        (for
         ((p+c (in-list sorted-player-cards)))
         (define player (first p+c))
         (define card (second p+c))
         (cond
          ((send deck larger-than-some-top-of-stacks? card)
           (define closest-fit-stack (send deck fit card))
           (cond
            ((< (length closest-fit-stack) FIVE) (send deck push card))
            ((= (length closest-fit-stack) FIVE)
             (define bulls (send deck replace closest-fit-stack card))
             (send player add-score bulls))))
          (else
           (define chosen-stack (send player choose deck))
           (define bulls (send deck replace chosen-stack card))
           (send player add-score bulls)))))))))
