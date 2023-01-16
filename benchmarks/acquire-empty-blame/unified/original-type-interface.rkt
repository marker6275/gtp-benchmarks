(module type-interface typed/racket/shallow
  (#%module-begin
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (define-type Hotel Any)
   (define-type Share Integer)
   (define-type Shares (HashTable Hotel Share))
   (define-type Color Symbol)
   (define-type Cash Natural)
   (define-type M*ority (U 'majority 'minority))
   (define-type Column Natural)
   (define-type Row Symbol)
   (define-type SpotType (U 'FOUNDING 'GROWING 'MERGING 'SINGLETON 'IMPOSSIBLE))
   (define-type Kind (U 'FOUNDING 'GROWING 'MERGING 'SINGLETON))
   (define-type Content (U Hotel 'UNTAKEN 'taken-no-hotel))
   (provide Hotel Share Shares Color Cash M*ority Column Row SpotType Kind Content)
   (provide Board Decisions Score Administrator% Turn% Player% RunResult Strategy)
   (reprovide (except-in "board-adapted.rkt" Board) (except-in "state-adapted.rkt" Decisions Score Administrator% Turn% Player% RunResult Strategy))
   (define-type Board (HashTable Tile Content))
   (define-type Decisions (Listof (List Player (Listof (List Hotel Boolean)))))
   (define-type Score (Listof (List String Cash)))
   (define-type
    Administrator%
    (Class
     (init-field (next-tile (-> (Listof Tile) Tile)))
     (sign-up (-> String (Instance Player%) String))
     (show-players (-> (Listof String)))
     (run (->* (Natural) (#:show (-> Void)) RunResult))))
   (define-type
    Turn%
    (Class
     (init-field (current-state State))
     (field (board Board) (current Player) (cash Cash) (tiles (Listof Tile)) (shares Shares) (hotels (Listof Hotel)) (players (Listof Player)))
     (reconcile-shares (-> Shares Shares))
     (eliminated (-> (Listof Player)))
     (place-called (-> Boolean))
     (decisions (-> (Values (Option Tile) (Option Hotel) Decisions)))
     (place (-> Tile Hotel (U Void (Listof Player))))))
   (define-type
    Player%
    (Class
     (init-field (name String) (choice Strategy))
     (field (*players (Listof Player)) (*bad (Listof Player)))
     (go (-> (Instance Administrator%) Void))
     (setup (-> State Void))
     (take-turn (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))
     (keep (-> (Listof Hotel) (Listof Boolean)))
     (receive-tile (-> Tile Void))
     (inform (-> State Void))
     (the-end (-> State Any Void))))
   (define-type RunResult (List (U 'done 'exhausted 'score 'IMPOSSIBLE) Any (Listof State)))
   (define-type Strategy (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))
   (require/typed/check/provide "admin.rkt" (administrator% Administrator%))
   (require/typed/check/provide "auxiliaries.rkt" (randomly-pick (All (A) (-> (Listof A) A))))
   (require/typed/check/provide
    "basics.rkt"
    (*combine-shares (-> (Listof Shares) Shares))
    (ALL-HOTELS (Listof Hotel))
    (SHARES-PER-TURN# Integer)
    (hotel<=? (-> Hotel Hotel Boolean))
    (price-per-share (-> Hotel Natural (Option Cash)))
    (shares-- (-> Shares Hotel Shares))
    (shares-available (-> Shares Hotel Share)))))
