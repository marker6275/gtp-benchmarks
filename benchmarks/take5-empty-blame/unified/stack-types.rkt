(module stack-types typed/racket/shallow
  (#%module-begin
   (require "../../../utilities/require-typed-check-provide-transient.rkt" "basics-types.rkt")
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (provide Stack)
   (define-type Stack (Listof Card))))
