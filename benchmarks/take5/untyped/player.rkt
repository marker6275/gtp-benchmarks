#lang racket/base

;; representing the player for an OO version of "6 Nimmt"

(provide
 ;; Name [x [Listof Card] -> [Listof Card]] -> Player
 ;; the default value for the second argument sorts the cards in descending order 
 create-player)

(require
  "take5-interface.rkt"
  racket/class
  (only-in racket/list first rest)
)

;; ---------------------------------------------------------------------------------------------------

(define (create-player i (order order:sort-by-face))
  (new player% [n i] [order order]))

(define (order:sort-by-face loc)
  (sort loc > #:key card-face))

