#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt"
         "../base/types.rkt")

(provide Board
         Decisions
         Score
         Administrator%
         Turn%
         Player%
         RunResult
         Strategy)

(reprovide "state-adapted.rkt"
           "board-adapted.rkt")


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
