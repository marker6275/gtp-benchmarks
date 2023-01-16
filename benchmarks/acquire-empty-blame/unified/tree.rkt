(module tree typed/racket/shallow
  (#%module-begin
   (provide tree-state lplaced% generate-tree tree-next hand-out? HandOut)
   (require "../../../utilities/require-typed-check-provide-transient.rkt" "../base/types.rkt" "board-adapted.rkt" "state-adapted.rkt")
   (require/typed/check "basics.rkt" (shares-available? (-> Shares (Listof Hotel) Boolean)) (shares-order? (-> (Listof Hotel) Boolean)))
   (define-type
    Tree<%>
    (Class
     (to-state (-> State))
     (next (-> Tile (Option Hotel) Decisions (Listof Hotel) (-> (Listof Tile) Tile) (Values (Option Tile) (Instance ATree%))))
     (founding (-> Natural (Listof (Listof Hotel)) Natural))
     (traversal (-> Natural (Listof (Listof Hotel)) (-> (Instance Placed%) Natural) Natural))
     (lookup-tile (-> (-> (Listof Tile) Tile) (Listof HandOut) (Values (Option Tile) (Instance ATree%))))
     (merging (-> Natural (Listof (Listof Hotel)) Natural))))
   (define-type ATree% (Class #:implements Tree<%> (init-field (state State)) (nothing-to-place? (-> Boolean))))
   (define-type
    Placed%
    (Class
     (init-field (state State) (tile Tile) (hotel (Option Hotel)) (state/tile State) (reason SpotType))
     (purchase (-> Decisions (Listof Hotel) (U (Instance ATree%) (Listof HandOut))))
     (to-trees (-> Decisions (Listof Hotel) (Listof (Instance ATree%))))
     (acceptable-policies (-> (Listof (Listof Hotel)) (Listof (Listof Hotel))))))
   (define-type LPlaced% (Class #:implements ATree% (init-field (lplaced (Listof (Instance Placed%))) (state State)) (nothing-to-place? (-> Boolean))))
   (struct hand-out ((tile : Tile) (tree : (Instance ATree%))))
   (define-type HandOut hand-out)
   (: placed-tile (-> (Instance Placed%) Tile))
   (define (placed-tile p) (get-field tile p))
   (: placed-hotel (-> (Instance Placed%) (Option Hotel)))
   (define (placed-hotel p) (get-field hotel p))
   (: atree% ATree%)
   (define atree%
     (class object%
       (init-field (state : State))
       (super-new)
       (define/public (nothing-to-place?) #f)
       (define/public (to-state) (get-field state this))
       (define/public (next t h d* h* pt) (error 'not-implemented))
       (define/public (founding n order-policies) (unless (andmap shares-order? order-policies) (error 'atree-founding "Precondition")) (traversal n order-policies (is-action FOUNDING)))
       (define/public (merging n order-policies) (traversal n order-policies (is-action MERGING)))
       (define/public (traversal n order-policies i) (error 'not-impolementd))
       (: is-action (-> Symbol (-> (Instance Placed%) (U 1 0))))
       (define ((is-action tag) p) (if (and (placed-hotel p) (eq? (get-field reason p) tag)) 1 0))
       (define/public (lookup-tile pick-tile lo-handout) (values #f this))))
   (: state% ATree%)
   (define state%
     (class atree%
       (super-new)
       (define/override (next . _) (error 'tree-next "finale state can't transition"))
       (define/override (traversal n policies p?) 0)
       (define/override (lookup-tile pick-tile lo-handout) (values #f this))))
   (: lplaced% LPlaced%)
   (define lplaced%
     (class atree%
       (super-new)
       (init-field (lplaced : (Listof (Instance Placed%))))
       (define/override (nothing-to-place?) (null? lplaced))
       (define/override
        (next tile hotel decisions shares-to-buy pick-tile)
        (define intermediate (send (lookup-purchase tile hotel) purchase decisions shares-to-buy))
        (unless (list? intermediate) (error "Expected a HandOut, got a State%"))
        (send this lookup-tile pick-tile intermediate))
       (: lookup-purchase (-> Tile (Option Hotel) (Instance Placed%)))
       (define/private
        (lookup-purchase t h)
        (or (for/or : (Option (Instance Placed%)) ((p : (Instance Placed%) lplaced) #:when (and (equal? (placed-hotel p) h) (equal? (placed-tile p) t))) p) (error 'noplace)))
       (define/override
        (lookup-tile pick-tile lo-hand-out)
        (define tile (pick-tile (map hand-out-tile lo-hand-out)))
        (define st (for/or : (Option (Instance ATree%)) ((p : HandOut lo-hand-out) #:when (equal? (hand-out-tile p) tile)) (hand-out-tree p)))
        (values tile (or st (error 'lookupfailed))))
       (define/override
        (traversal n policies p?)
        (if (= n 0)
          0
          (for/sum
           :
           Natural
           ((branch : (Instance Placed%) (in-list lplaced)))
           (define d* (map (lambda ((p : Player)) (list p '())) (state-players (get-field state/tile branch))))
           (define a (send branch acceptable-policies policies))
           (+ (p? branch) (if (empty? a) 0 (* (length a) (for/sum : Natural ((st : (Instance ATree%) (send branch to-trees d* (first a)))) (send st traversal (- n 1) policies p?))))))))))
   (: placed% Placed%)
   (define placed%
     (class object%
       (init-field state tile hotel state/tile reason)
       (super-new)
       (define/public
        (purchase decisions share-order)
        (when (eq? MERGING reason)
          (define players (state-players state/tile))
          (unless (= (length decisions) (length players)) (printf "contract failure: received wrong number of decisions") (error 'purchase "done")))
        (define state/decisions (if (eq? MERGING reason) (state-return-shares state/tile decisions (state-board state)) state/tile))
        (define state/bought (state-buy-shares state/decisions share-order))
        (define available-tiles (state-tiles state/bought))
        (if (empty? available-tiles)
          (new state% (state state/bought))
          (for/list : (Listof HandOut) ((tile : Tile available-tiles)) (hand-out tile (generate-tree (state-next-turn (state-move-tile state/bought tile)))))))
       (define/public
        (to-trees decisions share-order)
        (define state-or-hand-out (purchase decisions share-order))
        (cond ((list? state-or-hand-out) (map hand-out-tree state-or-hand-out)) (else (list state-or-hand-out))))
       (define/public
        (acceptable-policies policies)
        (unless (andmap shares-order? policies) (error 'acceptable-policies "Precondigion"))
        (define state state/tile)
        (define budget (player-money (state-current-player state)))
        (define board (state-board state))
        (define shares (state-shares state))
        (for/list ((p : (Listof Hotel) policies) #:when (and (shares-available? shares p) (affordable? board p budget))) p))))
   (: generate-tree (-> State (Instance ATree%)))
   (define (generate-tree state)
     (cond
      ((state-final? state) (new state% (state state)))
      (else
       (define board (state-board state))
       (define available-hotels (state-hotels state))
       (define lplaced
         (for/fold
          :
          (Listof (Instance Placed%))
          ((lo-placed : (Listof (Instance Placed%)) '()))
          ((t : Tile (player-tiles (state-current-player state))))
          (define kind (what-kind-of-spot board t))
          (: hotels (Listof (Option Hotel)))
          (define hotels
            (cond
             ((eq? kind IMPOSSIBLE) '())
             ((and (eq? FOUNDING kind) (cons? available-hotels)) available-hotels)
             ((eq? MERGING kind) (define-values (acquirers _) (merging-which board t)) acquirers)
             (else (list #f))))
          (define new-placements
            (for/list
             :
             (Listof (Instance Placed%))
             ((h : (Option Hotel) (in-list hotels)))
             (define state/tile (if h (state-place-tile state t h) (state-place-tile state t)))
             (new placed% (state state) (tile t) (hotel h) (state/tile state/tile) (reason kind))))
          (append new-placements lo-placed)))
       (new lplaced% (state state) (lplaced lplaced)))))
   (: tree-next (-> (Instance ATree%) Tile (Option Hotel) Decisions (Listof Hotel) (-> (Listof Tile) Tile) (Values (Option Tile) (Instance ATree%))))
   (define (tree-next current-tree tile hotel decisions shares-to-buy pick-tile) (send current-tree next tile hotel decisions shares-to-buy pick-tile))
   (: tree-state (-> (Instance ATree%) State))
   (define (tree-state t) (send t to-state))))
