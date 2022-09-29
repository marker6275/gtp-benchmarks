#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(reprovide "basics-types.rkt"
           "card-adapted.rkt"
           "card-pool-types.rkt"
           "dealer-types.rkt"
           "deck-types.rkt"
           "player-types.rkt"
           "stack-types.rkt")

(struct card (
 [face : Face]
 [bulls : Bulls])
  #:prefab
  #:type-name Card)

(provide (struct-out card)
         Card)

(require/typed/check/provide
 "dealer.rkt"
 [create-dealer (-> (Listof Player) Dealer)])

(require/typed/check/provide
 "base-player.rkt"
 [player% Player%]
 [default-order (-> (Listof Card) (Listof Card))])

