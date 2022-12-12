(module card-pool-types typed/racket/shallow
  (#%module-begin
   (provide CardPool% CardPool Hand)
   (require "../../../utilities/require-typed-check-provide-transient.rkt" typed/racket/class "card-adapted.rkt" "basics-types.rkt")
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (define-type
    CardPool%
    (Class
     (init-field (shuffle (-> (Listof Card) (Listof Card)) #:optional) (random-bulls (-> Bulls) #:optional))
     (get-field:shuffle (-> (-> (Listof Card) (Listof Card))))
     (get-field:random-bulls (-> (-> Bulls)))
     (set-field:shuffle (-> (-> (Listof Card) (Listof Card)) Void))
     (set-field:random-bulls (-> (-> Bulls) Void))
     (draw-card (-> Card))
     (draw-hand (-> Hand))))
   (define-type CardPool (Instance CardPool%))
   (define-type Hand (Listof Card))))
