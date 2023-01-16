(module mutation-adapter typed/racket/shallow
  (#%module-begin
   (module contracted racket
     (require (file "/home/lukas/github_sync/grad/projects/blame-gradual-typing/src/blame-evaluation-gt/mutation-adapter/mutation-adapter.rkt"))
     (require "original-type-interface.rkt")
     (define Administrator%
       (recursive-contract (delegating-class/c (list) (list) (list (cons 'sign-up (delegating-> 2 (list (cons 1 (delegating-instanceof Player%))) (any/c-adapter) (list)))))))
     (define Player%
       (recursive-contract
        (delegating-class/c
         (list)
         (list)
         (list
          (cons 'go (delegating-> 1 (list (cons 0 (delegating-instanceof Administrator%))) (any/c-adapter) (list)))
          (cons 'take-turn (delegating-> 1 (list) (any/c-adapter) (list (cons 1 (delegating-option (sealing-adapter))) (cons 2 (delegating-listof (sealing-adapter))))))
          (cons 'keep (delegating-> 1 (list (cons 0 (delegating-listof (sealing-adapter)))) (any/c-adapter) (list)))))))
     (provide (except-out (all-from-out "original-type-interface.rkt") *combine-shares ALL-HOTELS hotel<=? price-per-share shares-- shares-available administrator%))
     (begin
       (define *combine-shares1651428
         (contract
          (delegating->
           1
           (list (cons 0 (delegating-listof (delegating-hash/c (sealing-adapter) (any/c-adapter)))))
           (any/c-adapter)
           (list (cons 0 (delegating-hash/c (sealing-adapter) (any/c-adapter)))))
          *combine-shares
          #f
          #f))
       (provide (rename-out (*combine-shares1651428 *combine-shares))))
     (begin (define ALL-HOTELS1651429 (contract (delegating-listof (sealing-adapter)) ALL-HOTELS #f #f)) (provide (rename-out (ALL-HOTELS1651429 ALL-HOTELS))))
     (begin
       (define hotel<=?1651430 (contract (delegating-> 2 (list (cons 0 (sealing-adapter)) (cons 1 (sealing-adapter))) (any/c-adapter) (list)) hotel<=? #f #f))
       (provide (rename-out (hotel<=?1651430 hotel<=?))))
     (begin
       (define price-per-share1651431 (contract (delegating-> 2 (list (cons 0 (sealing-adapter))) (any/c-adapter) (list)) price-per-share #f #f))
       (provide (rename-out (price-per-share1651431 price-per-share))))
     (begin
       (define shares--1651432
         (contract
          (delegating->
           2
           (list (cons 0 (delegating-hash/c (sealing-adapter) (any/c-adapter))) (cons 1 (sealing-adapter)))
           (any/c-adapter)
           (list (cons 0 (delegating-hash/c (sealing-adapter) (any/c-adapter)))))
          shares--
          #f
          #f))
       (provide (rename-out (shares--1651432 shares--))))
     (begin
       (define shares-available1651433
         (contract (delegating-> 2 (list (cons 0 (delegating-hash/c (sealing-adapter) (any/c-adapter))) (cons 1 (sealing-adapter))) (any/c-adapter) (list)) shares-available #f #f))
       (provide (rename-out (shares-available1651433 shares-available))))
     (begin (define administrator%1651434 (contract Administrator% administrator% #f #f)) (provide (rename-out (administrator%1651434 administrator%)))))
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (provide Hotel Share Shares Color Cash M*ority Column Row SpotType Kind Content)
   (provide Board Decisions Score Administrator% Turn% Player% RunResult Strategy)
   (reprovide (except-in "board-adapted.rkt" Board) (except-in "state-adapted.rkt" Decisions Score Administrator% Turn% Player% RunResult Strategy))
   (define-type Hotel String)
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
   (require/typed/check/provide 'contracted (administrator% Administrator%))
   (require/typed/check/provide 'contracted (randomly-pick (All (A) (-> (Listof A) A))))
   (require/typed/check/provide
    'contracted
    (*combine-shares (-> (Listof Shares) Shares))
    (ALL-HOTELS (Listof Hotel))
    (SHARES-PER-TURN# Integer)
    (hotel<=? (-> Hotel Hotel Boolean))
    (price-per-share (-> Hotel Natural (Option Cash)))
    (shares-- (-> Shares Hotel Shares))
    (shares-available (-> Shares Hotel Share)))))
