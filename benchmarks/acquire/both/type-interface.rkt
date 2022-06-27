#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt"
         (only-in "state-adapted.rkt"
                  Player
                  player?
                  player-money
                  player-tiles
                  player-shares
                  player-external
                  player-name
                  State
                  state?
                  state-players)
         (only-in "board-adapted.rkt"
                  Tile
                  tile?)
         "../base/types.rkt")

(provide (all-from-out "state-adapted.rkt")
         (all-from-out "board-adapted.rkt")
         Board
         Decisions
         Score
         Administrator%
         Turn%
         Player%
         RunResult
         Strategy)


(define-type Board (HashTable Tile Content))

(define-type Decisions (Listof (List Player (Listof (List Hotel Boolean)))))
(define-type Score (Listof (List String Cash)))

(define-type Administrator%
  (Class
   (init-field
    (next-tile (-> (Listof Tile) Tile)))
   (sign-up (-> String (Instance Player%) String))
   (show-players (-> (Listof String)))
   (run (->* (Natural) (#:show (-> Void)) RunResult))
   ))

(define-type Turn%
  (Class
   (init-field (current-state State))
   (field
    (board Board)
    (current Player)
    (cash Cash)
    (tiles (Listof Tile))
    (shares Shares)
    (hotels (Listof Hotel))
    (players (Listof Player)))
   (reconcile-shares (-> Shares Shares))
   (eliminated (-> (Listof Player)))
   (place-called (-> Boolean))
   (decisions (-> (Values (Option Tile) (Option Hotel) Decisions)))
   ;; Precondition: (send this place-called)
   (place (-> Tile Hotel (U Void (Listof Player))))
   ))

(define-type Player%
  (Class
   (init-field
    [name String]
    [choice Strategy])
   (field
    [*players (Listof Player)]
    [*bad (Listof Player)])
   (go (-> (Instance Administrator%) Void))
   (setup (-> State Void))
   (take-turn (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))
   (keep (-> (Listof Hotel) (Listof Boolean)))
   (receive-tile (-> Tile Void))
   (inform (-> State Void))
   (the-end (-> State Any Void))))

(define-type RunResult (List (U 'done 'exhausted 'score 'IMPOSSIBLE) Any (Listof State)))
(define-type Strategy (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))


(require/typed/check/provide "state.rkt"
  (*create-player (-> String Cash Shares (Listof Tile) Player))
  (player0 (-> String Tile Tile Tile Tile Tile Tile (Instance Player%) Player))
  (state0 (-> Player * State))
  (state-sub-shares (-> State Shares State))
  (*cs0 (-> String * State))
  (*create-state (-> Board (Listof Player) State))
  (state-place-tile (->* (State Tile) ((Option Hotel)) State))
  (state-move-tile (-> State Tile State))
  (state-next-turn (-> State State))
  (state-remove-current-player (-> State State))
  (state-eliminate (-> State (Listof Player) State))
  (state-current-player (-> State Player))
  (state-buy-shares (-> State (Listof Hotel) State))
  (state-return-shares (->* [State Decisions] [Board] State))
  (state-score (-> State (Listof (List String Cash))))
  (state-final? (-> State Boolean))
)

(require/typed/check/provide "board.rkt"
  (tile<=? (-> Tile Tile Boolean))
  (tile->string (-> Tile String))
  (ALL-TILES (Listof Tile))
  (STARTER-TILES# Natural)
  (FOUNDING 'FOUNDING)
  (GROWING 'GROWING)
  (MERGING 'MERGING)
  (SINGLETON 'SINGLETON)
  (IMPOSSIBLE 'IMPOSSIBLE)
  (deduplicate/hotel (-> (Listof Hotel) (Listof Hotel)))
  (make-board (-> Board))
  (board-tiles (-> Board (Listof Tile)))
  (what-kind-of-spot (-> Board Tile SpotType))
  (growing-which (-> Board Tile (Option Hotel)))
  (merging-which (-> Board Tile (Values (Pairof Hotel (Listof Hotel)) (Listof Hotel))))
  (size-of-hotel (-> Board Hotel Natural))
  (free-spot? (-> Board Tile Boolean))
  (merge-hotels (-> Board Tile Hotel Board))
  (found-hotel (-> Board Tile Hotel Board))
  (grow-hotel (-> Board Tile Board))
  (place-tile (-> Board Tile Board))
  (set-board (-> Board Tile Kind (Option Hotel) Board))
  (affordable? (-> Board (Listof Hotel) Cash Boolean))
  (*create-board-with-hotels (-> (Listof Tile) (Listof (Pairof Hotel (Listof Tile))) Board))
  (distinct-and-properly-formed (-> (Listof Tile) (-> (Listof (Pairof Hotel (Listof Tile))) Boolean)))
)

(require/typed/check/provide
 "admin.rkt"
 [administrator% Administrator%]
 [turn% Turn%])

(require/typed/check/provide
 "auxiliaries.rkt"
 [randomly-pick (All (A) (-> (Listof A) A))])

(require/typed/check/provide
 "basics.rkt"
 [player-shares0 Shares]
 [*combine-shares (-> (Listof Shares) Shares)]
 [shares-minus (-> Shares Shares Shares)]
 [banker-shares0 Shares]
 [ALL-HOTELS (Listof Hotel)]
 [SHARES-PER-TURN# Integer]
 [hotel<=? (-> Hotel Hotel Boolean)]
 [price-per-share (-> Hotel Natural (Option Cash))]
 [shares++ (-> Shares Hotel Shares)]
 [shares-- (-> Shares Hotel Shares)]
 [shares-available (-> Shares Hotel Share)])
