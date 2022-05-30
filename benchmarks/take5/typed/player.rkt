#lang typed/racket/base

;; representing the player for an OO version of "6 Nimmt"

(provide
 ;; Name [x [Listof Card] -> [Listof Card]] -> Player
 ;; the default value for the second argument sorts the cards in descending order 
 create-player)

(require
  "type-interface.rkt"
  typed/racket/class)

;; ---------------------------------------------------------------------------------------------------

(: create-player (->* [Name] [(-> (Listof Card) (Listof Card))] Player))
(define (create-player i (order order:sort-by-face))
  (new player% [n i] [order order]))

(: order:sort-by-face (-> (Listof Card) (Listof Card)))
(define (order:sort-by-face loc)
  ((inst sort Card Natural) loc > #:key card-face))

