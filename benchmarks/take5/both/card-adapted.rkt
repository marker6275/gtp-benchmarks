#lang typed/racket/base

(provide
  ;; Card
  ;; card
  ;; card-face
  ;; card-bulls
  >-face
  --face)

(require
  require-typed-check
  "basics-types.rkt")
(struct card (
 [face : Face]
 [bulls : Bulls])
  #:prefab
  #:type-name Card)

(require/typed/check "card.rkt"
 (>-face (-> Card Card Boolean))
 (--face (-> Card Card Natural)))

