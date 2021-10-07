#lang racket/base

;; representing the player for an OO version of "6 Nimmt"

(provide
 ;; Name [x [Listof Card] -> [Listof Card]] -> Player
 ;; the default value for the second argument sorts the cards in descending order 
 default-order

 ;; for sub-typing:
 ;; Player%
 player%)

(require
  racket/class
  (only-in racket/list first rest)
)

;; ---------------------------------------------------------------------------------------------------

(define (default-order loc) loc)

(define player%
  (class object%
    (init-field n
      (order  default-order))

    (field [my-cards '()])

    (define/public (name) n)

    (define/public (start-round loc)
      (set! my-cards (order loc)))

    (define/public (start-turn _d)
      (begin0 (first my-cards)
              (set! my-cards (rest my-cards))))

    (define/public (choose d)
      (define fewest-bulls (send d fewest-bulls))
      fewest-bulls)

    (super-new)))
