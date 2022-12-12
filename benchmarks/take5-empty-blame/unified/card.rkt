(module card typed/racket/shallow
  (#%module-begin
   (provide (struct-out card) >-face --face)
   (require "../../../utilities/require-typed-check-provide-transient.rkt" "basics-types.rkt")
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (: >-face (-> Card Card Boolean))
   (define (>-face c d) (> (card-face c) (card-face d)))
   (: face? (-> Any Boolean : #:+ Face))
   (define (face? f) (and (exact-nonnegative-integer? f) (< 0 f) (< f 105)))
   (: --face (-> Card Card Face))
   (define (--face c d) (assert (- (card-face c) (card-face d)) face?))))
