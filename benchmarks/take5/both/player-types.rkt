#lang typed/racket/base

(provide
  Player%
  Player)

(require
  "basics-types.rkt"
  "deck-types.rkt"
  "card-adapted.rkt"
  "stack-types.rkt"
  typed/racket/class)
(struct card (
 [face : Face]
 [bulls : Bulls])
  #:prefab
  #:type-name Card)


(define-type Player%
  (Class
    (init-field
      (n Name)
      (order (-> (Listof Card) (Listof Card)) #:optional))
    (field [my-cards [Listof Card]])
    (get-field:n (-> Name))
    (get-field:order (-> (-> (Listof Card) (Listof Card))))
    (get-field:my-cards (-> [Listof Card]))
    (set-field:n (-> Name Void))
    (set-field:order (-> (-> (Listof Card) (Listof Card)) Void))
    (set-field:my-cards (-> [Listof Card] Void))

    (name (-> Name))
    (start-round (-> (Listof Card) Void))
    (start-turn (-> Deck Card))
    (choose (-> Deck Stack))))
(define-type Player (Instance Player%))
