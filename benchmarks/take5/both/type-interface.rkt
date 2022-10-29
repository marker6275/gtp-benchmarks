#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(reprovide ;; "basics-types.rkt"
           "card-adapted.rkt"
           ;; "card-pool-types.rkt"
           ;; "dealer-types.rkt"
           ;; "deck-types.rkt"
           ;; "player-types.rkt"
           ;; "stack-types.rkt"
           )

(provide Name
         Face
         Bulls)
(define-type Name Natural)
(define-type Face Natural)
(define-type Bulls Natural)

(provide CardPool%
         CardPool
         Hand)
(define-type CardPool%
  (Class
    (init-field
      (shuffle (-> (Listof Card) (Listof Card)) #:optional)
      (random-bulls (-> Bulls) #:optional))
    (get-field:shuffle (-> (-> (Listof Card) (Listof Card))))
    (get-field:random-bulls (-> (-> Bulls)))
    (set-field:shuffle (-> (-> (Listof Card) (Listof Card)) Void))
    (set-field:random-bulls (-> (-> Bulls) Void))

    (draw-card
     ;; effect: pick and return one card from the pool of cards
     (-> Card))
    (draw-hand
     ;; effect: pick and return HAND cards from the pool of cards
     (-> Hand))))
(define-type CardPool (Instance CardPool%))
(define-type Hand (Listof Card))


(provide Stack)
(define-type Stack
  (Listof Card))

(provide BaseDeck%
         PlayerDeck%
         DealerDeck%
         Deck%
         BaseDeck
         PlayerDeck
         DealerDeck
         Deck)

(define-type BaseDeck%
    (Class
      (init-field (cards0 (Listof Card)))
      (field (my-stacks (Listof Stack)))
      (get-field:cards0 (-> (Listof Card)))
      (get-field:my-stacks (-> (Listof Stack)))
      (set-field:cards0 (-> (Listof Card) Void))
      (set-field:my-stacks (-> (Listof Stack) Void))))
(define-type PlayerDeck%
    (Class ;; for player
      #:implements/inits BaseDeck%
      (fewest-bulls (-> Stack))))
(define-type DealerDeck%
    (Class ;; for dealer
      #:implements/inits BaseDeck%
      (fit (-> Card Stack))
      (push (-> Card Void))
      (replace (-> Stack Card Natural))
      (replace-stack (-> Card (U Card (Listof Card)) Natural))
      (larger-than-some-top-of-stacks? (-> Card Boolean))))
(define-type Deck%
    (Class
      #:implements/inits BaseDeck%
      (fewest-bulls (-> Stack))
      (fit (-> Card Stack))
      (push (-> Card Void))
      (replace (-> Stack Card Natural))
      (replace-stack (-> Card (U Card (Listof Card)) Natural))
      (larger-than-some-top-of-stacks? (-> Card Boolean))))

(define-type BaseDeck (Instance BaseDeck%))
(define-type PlayerDeck (Instance PlayerDeck%))
(define-type DealerDeck (Instance DealerDeck%))
(define-type Deck (Instance Deck%))


(provide Result
         Internal%
         Internal
         Dealer%
         Dealer)
(define-type Result (List (List Symbol Natural) (Listof (List Name Natural))))

;; (sad face)
(define-type Internal%
  (Class
    #:implements Player%
    (init-field [player Player])
    (field [my-bulls Natural])
    (get-field:player (-> Player))
    (get-field:my-bulls (-> Natural))
    (set-field:player (-> Player Void))
    (set-field:my-bulls (-> Natural Void))

    (bulls (-> Natural))
    (add-score (-> Natural Void))))
(define-type Internal (Instance Internal%))

(define-type Dealer%
  (Class
    (init-field (players (Listof Player)))
    (field
     (internal% Internal%)
     (internals (Listof Internal)))
    (get-field:players (-> (Listof Player)))
    (get-field:internal% (-> Internal%))
    (get-field:internals (-> (Listof Internal)))
    (set-field:players (-> (Listof Player) Void))
    (set-field:internal% (-> Internal% Void))
    (set-field:internals (-> (Listof Internal) Void))

    (present-results (-> Natural Result))
    (any-player-done? (-> Boolean))
    (play-round (-> (-> (Listof Card) (Listof Card)) (-> Bulls) Void))
    (play-game (->* ()  ((-> (Listof Card) (Listof Card)) (-> Bulls)) Result))))
(define-type Dealer (Instance Dealer%))


(provide Player%
         Player)
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
 ;; Unused by client
 #;[default-order (-> (Listof Card) (Listof Card))])

