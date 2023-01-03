(module base-player typed/racket/shallow
  (#%module-begin
   (provide default-order player%)
   (require "../../../utilities/require-typed-check-provide-transient.rkt"
            "basics-types.rkt"
            "card-adapted.rkt"
            "deck-types.rkt"
            "player-types.rkt"
            "stack-types.rkt"
            typed/racket/class
            (only-in racket/list first rest))
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (: default-order (-> (Listof Card) (Listof Card)))
   (define (default-order loc) loc)
   (: player% Player%)
   (define player%
     (class object%
       (init-field n (order default-order))
       (field (my-cards '()))
       (define/public (get-field:n) n)
       (define/public (set-field:n v) (set! n v))
       (define/public (get-field:order) order)
       (define/public (set-field:order v) (set! order v))
       (define/public (get-field:my-cards) my-cards)
       (define/public (set-field:my-cards v) (set! my-cards v))
       (define/public (name) n)
       (define/public (start-round loc) (set! my-cards (order #;(get-field order this) loc)))
       (define/public (start-turn _d) (begin0 (first my-cards) (set! my-cards (rest my-cards))))
       (define/public (choose d) (define fewest-bulls (send d fewest-bulls)) fewest-bulls)
       (super-new)))))
