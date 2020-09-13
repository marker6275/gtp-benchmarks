#lang racket/base

;; representing the deck of STACKS stack on the table 

(provide
 ;; CardPool -> Deck 
 create-deck)

;; -----------------------------------------------------------------------------

(require racket/class
         "for-dealer.rkt"
         "for-player.rkt")

(require (only-in "basics.rkt"
                  STACKS))

;; ---------------------------------------------------------------------------------------------------

(define (create-deck card-pool)
  (define deck% (for-player (for-dealer base-deck%)))
  (define cards (build-list STACKS (lambda (_) (send card-pool draw-card))))
  (new deck% [cards0 cards]))

