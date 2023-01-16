(module card-adapted typed/racket/shallow
  (#%module-begin
   (provide >-face --face)
   (require "../../../utilities/require-typed-check-provide-transient.rkt" "basics-types.rkt")
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (require/typed/check
    "card.rkt"
    (>-face (-> Card Card Boolean))
    (--face (-> Card Card Natural)))))
