#lang typed/racket/base

;; representing the deck of STACKS stack on the table 

(provide
 ;; CardPool -> Deck 
 create-deck)

;; -----------------------------------------------------------------------------

(require
  typed/racket/class
  "basics-types.rkt"
  "card-adapted.rkt"
  "card-pool-types.rkt"
  "deck-types.rkt"
  "stack-types.rkt"
  racket/list
  require-typed-check
)

(require/typed/check "for-dealer.rkt"
                     [for-dealer (-> BaseDeck% DealerDeck%)]
                     [base-deck% BaseDeck%])

(require/typed/check "for-player.rkt"
                     [for-player (-> DealerDeck% Deck%)])

(require/typed/check "basics.rkt"
  (STACKS Natural))

;; ---------------------------------------------------------------------------------------------------

(: create-deck (-> CardPool Deck))
(define (create-deck card-pool)
  (define deck% (for-player (for-dealer base-deck%)))
  (define cards (build-list STACKS (lambda (_) (send card-pool draw-card))))
  (new deck% [cards0 cards]))

