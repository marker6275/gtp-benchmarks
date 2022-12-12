(module card-pool racket/base
  (#%module-begin
   (provide create-card-pool)
   (require racket/class "../base/untyped.rkt" (only-in racket/list shuffle first rest))
   (require "card.rkt")
   (struct card (face bulls) #:prefab)
   (require (only-in "basics.rkt" FACE HAND MIN-BULL MAX-BULL))
   (define (create-card-pool (shuffle shuffle) (random-bulls random-bulls)) (new card-pool% (shuffle shuffle) (random-bulls random-bulls)))
   (define rng (vector->pseudo-random-generator '#(12 34 56 78 90 1)))
   (define (random-bulls) (random MIN-BULL (+ MAX-BULL 1) rng))
   (define card-pool%
     (class object%
       (init-field (shuffle shuffle) (random-bulls random-bulls))
       (define/public (get-field:shuffle) shuffle)
       (define/public (get-field:random-bulls) random-bulls)
       (define/public (set-field:shuffle v) (set! shuffle v))
       (define/public (set-field:random-bulls v) (set! random-bulls v))
       (super-new)
       (define my-cards (shuffle (build-list FACE (lambda (i) (card (+ i 1) (random-bulls))))))
       (define/public (draw-card) (begin0 (first my-cards) (set! my-cards (rest my-cards))))
       (define/public (draw-hand) (build-list HAND (lambda (_) (draw-card))))))))
