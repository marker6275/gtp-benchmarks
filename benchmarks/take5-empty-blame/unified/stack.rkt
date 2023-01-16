(module stack typed/racket/shallow
  (#%module-begin
   (provide create-stack top push length bulls)
   (require "../../../utilities/require-typed-check-provide-transient.rkt"
            "card-adapted.rkt"
            "stack-types.rkt"
            "basics-types.rkt")
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (require "../../../utilities/require-typed-check-provide-transient.rkt"
            (prefix-in list: (only-in racket/base length)))
   (require "../../../utilities/require-typed-check-provide-transient.rkt"
            (only-in racket/list first))
   (: create-stack (-> Card Stack))
   (define (create-stack c) (list c))
   (: top (-> Stack Card))
   (define top first)
   (: push (-> Card Stack Stack))
   (define (push c s) ((inst cons Card Card) c s))
   (: length (-> Stack Natural))
   (define length list:length)
   (: bulls (-> Stack Natural))
   (define (bulls s) (foldr + 0 ((inst map Bulls Card) card-bulls s)))))
