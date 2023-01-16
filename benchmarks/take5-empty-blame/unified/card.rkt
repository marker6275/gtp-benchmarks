(module card racket/base
  (#%module-begin
   (provide >-face --face)
   (require "../base/untyped.rkt")
   (struct card (face bulls) #:prefab)
   (define (>-face c d) (> (card-face c) (card-face d)))
   (define (face? f) (and (exact-nonnegative-integer? f) (< 0 f) (< f 105)))
   (define (--face c d) (assert (- (card-face c) (card-face d)) face?))))
