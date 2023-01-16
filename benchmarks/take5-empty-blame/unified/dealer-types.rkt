(module dealer-types typed/racket/shallow
  (#%module-begin
   (provide Result Dealer% Dealer Internal% Internal)
   (require "../../../utilities/require-typed-check-provide-transient.rkt"
            typed/racket/class
            "basics-types.rkt"
            "card-adapted.rkt"
            "player-types.rkt")
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (define-type Result (List (List Symbol Natural) (Listof (List Name Natural))))
   (define-type
    Internal%
    (Class
     #:implements
     Player%
     (init-field (player Player))
     (field (my-bulls Natural))
     (get-field:player (-> Player))
     (get-field:my-bulls (-> Natural))
     (set-field:player (-> Player Void))
     (set-field:my-bulls (-> Natural Void))
     (bulls (-> Natural))
     (add-score (-> Natural Void))))
   (define-type Internal (Instance Internal%))
   (define-type
    Dealer%
    (Class
     (init-field (players (Listof Player)))
     (field (internal% Internal%) (internals (Listof Internal)))
     (get-field:players (-> (Listof Player)))
     (get-field:internal% (-> Internal%))
     (get-field:internals (-> (Listof Internal)))
     (set-field:players (-> (Listof Player) Void))
     (set-field:internal% (-> Internal% Void))
     (set-field:internals (-> (Listof Internal) Void))
     (present-results (-> Natural Result))
     (any-player-done? (-> Boolean))
     (play-round (-> (-> (Listof Card) (Listof Card)) (-> Bulls) Void))
     (play-game (->* () ((-> (Listof Card) (Listof Card)) (-> Bulls)) Result))))
   (define-type Dealer (Instance Dealer%))))
