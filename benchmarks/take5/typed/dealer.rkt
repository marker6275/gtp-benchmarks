#lang typed/racket/base

;; the dealer and supervisor of the deck

(provide
 ;; [Listof Player] -> Dealer
 ;; create a dealer object that connects the players with the deck
 ;; and places the player's chosen cards
 create-dealer)

(require
  "basics-types.rkt"
  "card-adapted.rkt"
  "card-pool-types.rkt"
  "dealer-types.rkt"
  "deck-types.rkt"
  "player-types.rkt"
  racket/list
  require-typed-check
  typed/racket/class)

(struct card (
 [face : Face]
 [bulls : Bulls])
  #:prefab
  #:type-name Card)


(require/typed/check "basics.rkt"
  (FACE  Natural)
  (FIVE  Natural)
  (STACKS Natural)
  (SIXTYSIX Natural)
  (HAND  Natural)
  (MIN-BULL Bulls)
  (MAX-BULL Bulls)
  (configuration (-> (Listof (List Symbol Natural))))
)
(require/typed/check "card-pool.rkt"
  (create-card-pool (-> (-> (Listof Card) (Listof Card)) (-> Bulls) CardPool))
)
(require/typed/check "deck.rkt"
  (create-deck (-> CardPool Deck))
)
(require/typed/check "base-player.rkt"
  (player% Player%)
)

;; ---------------------------------------------------------------------------------------------------

;; TODO should not need to supply this
(: default-order (-> (Listof Card) (Listof Card)))
(define (default-order loc)
  ((inst sort Card Natural) loc > #:key card-face))

(: default-faces (-> Bulls))
(define (default-faces)
  MIN-BULL)

;; Note to self: the types for the below descriptions are used out of scope for now
;; in a file-module they come back into scope 

(: create-dealer (-> (Listof Player) Dealer))
(define (create-dealer players)
  (new dealer% [players players]))

(define dealer% : Dealer%
  (class object%
    (init-field
     (players : (Listof Player)))

    (super-new)

    (field
     [internal% : Internal%
      (class object%
        (super-new)
        (init-field player)
        (field [n (send player name)]
               [order default-order]
               [my-cards empty]

               [my-bulls 0])
        (define/public (name) n)
        (define/public (start-round cs) (send player start-round cs))
        (define/public (start-turn d) (send player start-turn d))
        (define/public (choose d) (send player choose d))

        (define/public (bulls) (send this get-field:my-bulls))
        (define/public (add-score n)
          (send this set-field:my-bulls (+ n (send this get-field:my-bulls))))

        (define/public (get-field:n) (send player get-field:n))
        (define/public (set-field:n v) (send player set-field:n v))
        (define/public (get-field:order) (send player get-field:order))
        (define/public (set-field:order v) (send player set-field:order v))
        (define/public (get-field:my-cards) (send player get-field:my-cards))
        (define/public (set-field:my-cards v) (send player set-field:my-cards v))

        (define/public (get-field:player) player)
        (define/public (get-field:my-bulls) my-bulls)
        (define/public (set-field:player v) (set! player v))
        (define/public (set-field:my-bulls v) (set! my-bulls v)))]
     [internals (for/list : (Listof Internal)
                          ([p : Player (in-list (send this get-field:players))])
                  (new internal% [player p]))])

    (define/public (get-field:players) players)
    (define/public (get-field:internal%) internal%)
    (define/public (get-field:internals) internals)
    (define/public (set-field:players v) (set! players v))
    (define/public (set-field:internal% v) (set! internal% v))
    (define/public (set-field:internals v) (set! internals v))

    ;; ---------------------------------------------------------------------------------------------
    ;; running a game 

    (define/public (play-game (shuffle values) (faces default-faces))
      (define n (length (send this get-field:internals)))
      (when (> (+ (* n HAND) STACKS) FACE)
        (error 'play-game "cannot play with ~a players; more cards needed" n))

      (let play-game : Result ([i : Natural 1])
        (play-round shuffle faces)
        (if (any-player-done?)
            (present-results i)
            (play-game (+ i 1)))))

    (define/public (present-results i)
      (define sorted
        ((inst sort Internal Natural)
         (send this get-field:internals) < #:key (lambda ([i : Internal]) (send i bulls))))
      `((after-round ,i)
        ,(for/list : (Listof (List Name Natural))
            ([p : Internal (in-list sorted)])
           `(,(send p name) ,(send p bulls)))))

    (define/public (any-player-done?)
      (for/or : Boolean
              ((p : Internal (in-list (send this get-field:internals))))
        (> (send p bulls) SIXTYSIX)))

    (define/public (play-round shuffle faces)
      (define card-pool (create-card-pool shuffle faces))
      (define deck (create-deck card-pool))
      (deal-cards card-pool)
      (for ((p HAND))
        (play-turn deck)))

    (: deal-cards (-> CardPool Void))
    (define/private (deal-cards card-pool)
      (for ((p : Internal (in-list (send this get-field:internals))))
        (send p start-round (send card-pool draw-hand))))

    (: play-turn (-> Deck Void))
    (define/private (play-turn deck)
      (define played-cards
        (for/list : (Listof (List Internal Card))
                  ((p : Internal (in-list (send this get-field:internals))))
          (list p (send p start-turn deck))))
      (define sorted-played-cards
        ((inst sort (List Internal Card) Face) played-cards < #:key (lambda ([x : (List Internal Card)]) (card-face (second x)))))
      (place-cards deck sorted-played-cards))

    (: place-cards (-> Deck (Listof (List Internal Card)) Void))
    (define/private (place-cards deck sorted-player-cards)
      (for ((p+c : (List Internal Card) (in-list sorted-player-cards)))
        (define player (first p+c))
        (define card (second p+c))
        (cond
          [(send deck larger-than-some-top-of-stacks? card)
           (define closest-fit-stack (send deck fit card))
           (cond
             [(< (length closest-fit-stack) FIVE)
              (send deck push card)]
             [(= (length closest-fit-stack) FIVE)
              (define bulls (send deck replace closest-fit-stack card))
              (send player add-score bulls)])]
          [else ;; the tops of all stacks have larger face values than card
           (define chosen-stack (send player choose deck))
           (define bulls (send deck replace chosen-stack card))
           (send player add-score bulls)])))

))
