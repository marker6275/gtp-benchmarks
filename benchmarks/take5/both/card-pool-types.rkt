#lang typed/racket/base

(provide
  CardPool%
  CardPool
  Hand)

(require
  typed/racket/class
  "card-adapted.rkt"
  "basics-types.rkt")

(struct card (
 [face : Face]
 [bulls : Bulls])
  #:prefab
  #:type-name Card)


(define-type CardPool%
  (Class
    (init-field
      (shuffle (-> (Listof Card) (Listof Card)) #:optional)
      (random-bulls (-> Bulls) #:optional))
    (draw-card
     ;; effect: pick and return one card from the pool of cards
     (-> Card))
    (draw-hand
     ;; effect: pick and return HAND cards from the pool of cards
     (-> Hand))))
(define-type CardPool (Instance CardPool%))
(define-type Hand (Listof Card))
