(module card-pool typed/racket/shallow
  (#%module-begin
   (provide create-card-pool)
   (require "../../../utilities/require-typed-check-provide-transient.rkt"
            "card-adapted.rkt"
            "basics-types.rkt"
            "card-pool-types.rkt"
            typed/racket/class
            (only-in racket/list shuffle first rest))
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (require/typed/check
    "basics.rkt"
    (FACE Natural)
    (HAND Natural)
    (MIN-BULL Natural)
    (MAX-BULL Natural))
   (: create-card-pool (->* () ((-> (Listof Card) (Listof Card)) (-> Bulls)) CardPool))
   (define (create-card-pool (shuffle shuffle) (random-bulls random-bulls))
     (new card-pool% (shuffle shuffle) (random-bulls random-bulls)))
   (define rng (vector->pseudo-random-generator '#(12 34 56 78 90 1)))
   (: random-bulls (-> Bulls))
   (define (random-bulls) (random MIN-BULL (+ MAX-BULL 1) rng))
   (: card-pool% CardPool%)
   (define card-pool%
     (class object%
       (init-field (shuffle shuffle) (random-bulls random-bulls))
       (define/public (get-field:shuffle) shuffle)
       (define/public (get-field:random-bulls) random-bulls)
       (define/public (set-field:shuffle v) (set! shuffle v))
       (define/public (set-field:random-bulls v) (set! random-bulls v))
       (super-new)
       (define
        my-cards
        :
        (Listof Card)
        (shuffle (build-list FACE (lambda ((i : Natural)) (card (+ i 1) (random-bulls))))))
       (define/public (draw-card) (begin0 (first my-cards) (set! my-cards (rest my-cards))))
       (define/public (draw-hand) (build-list HAND (lambda (_) (draw-card))))))))
