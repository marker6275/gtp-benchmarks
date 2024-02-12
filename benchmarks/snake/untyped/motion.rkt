#lang racket

(require (except-in "data.rkt" posn=?)
         (except-in "const.rkt" WORLD FOOD-RADIUS SEGMENT-RADIUS BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS BOARD-WIDTH BOARD-HEIGHT GRID-SIZE)
         (except-in "motion-help.rkt" snake-grow snake-slither next-head)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         modalc
         "../../curr-mode.rkt")
(require/configurable-contract "motion-help.rkt" snake-grow snake-slither next-head )
(require/configurable-contract "const.rkt" WORLD FOOD-RADIUS SEGMENT-RADIUS BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS BOARD-WIDTH BOARD-HEIGHT GRID-SIZE )
(require/configurable-contract "data.rkt" posn=? )

(provide/configurable-contract
 [r ([max pseudo-random-generator?]
     [types pseudo-random-generator?])]
 ;; ->* contract? no modal?
 [reset! ([max (modal->* curr-mode ()
                    void?
                    #:post
                    (equal?
                     (pseudo-random-generator->vector r)
                     (let ([r* (make-pseudo-random-generator)])
                       (parameterize ([current-pseudo-random-generator r*])
                         (random-seed 1324)
                         (pseudo-random-generator->vector r*)))))]
          [types (modal-> curr-mode void?)])]
 [world->world ([max (modal->i curr-mode ([w world-type?])
                          [result
                           (w)
                           (match w
                             [(world (snake dir (cons h t)) food) #:when (posn=? h food)
                                                                  (world/c (snake/c dir
                                                                                    (compose (=/c (+ (length t) 2))
                                                                                             length))
                                                                           any/c)]
                             [(world (snake dir segs) food)
                              (world/c (snake/c dir
                                                (compose (=/c (length segs))
                                                         length))
                                       (food=?/c food))])])]
                [types (curr-mode world-type? . modal-> . world-type?)])]
 [eating? ([max (modal->i curr-mode ([w world-type?])
                     [result (w)
                             (match w
                               [(world (snake _ (cons p1 _)) p2) #:when (posn=? p1 p2)
                                                                 #t]
                               [_ #f])])]
           [types (curr-mode world-type? . modal-> . boolean?)])]
 [snake-change-direction ([max (modal->i curr-mode ([snk snake-type?]
                                     [dir snake-dir?])
                                    [result (snk dir)
                                            (snake/c dir (snake-segs=?/c (snake-segs snk)))])]
                          [types (curr-mode snake-type? string? . modal-> . snake-type?)])]
 [world-change-dir ([max (modal->i curr-mode ([w world-type?]
                               [dir snake-dir?])
                              [result (w dir)
                                      (match w
                                        [(world (snake _ segs) food)
                                         (world/c (snake/c dir (snake-segs=?/c segs))
                                                  (food=?/c food))])])]
                    [types (curr-mode world-type? string? . modal-> . world-type?)])]
 [snake-eat ([max (modal->i curr-mode ([w world-type?])
                       [result (w)
                               (world/c (snake=?/c (snake-grow (world-snake w)))
                                        (posn/c (integer-in 0 (sub1 BOARD-WIDTH))
                                                (integer-in 0 (sub1 BOARD-HEIGHT))))])]
             [types (curr-mode world-type? . modal-> . world-type?)])])


;; (provide reset!)
;; (provide
;;  world-change-dir
;;  world->world)


(define r (make-pseudo-random-generator))
(define (reset!)
  (parameterize ((current-pseudo-random-generator r))
    (random-seed 1324)))

;; world->world : World -> World
(define (world->world w)
  (cond [(eating? w) (snake-eat w)]
        [else
         (world (snake-slither (world-snake w))
                (world-food w))]))
;; eating? : World -> Boolean
;; Is the snake eating the food in the world.
(define (eating? w)
  (posn=? (world-food w)
          (car (snake-segs (world-snake w)))))
;; snake-change-direction : Snake Direction -> Snake
;; Change the direction of the snake.
(define (snake-change-direction snk dir)
  (snake dir
         (snake-segs snk)))
;; world-change-dir : World Direction -> World
;; Change direction of the world.
(define (world-change-dir w dir)
  (world (snake-change-direction (world-snake w) dir)
         (world-food w)))



;; snake-eat : World -> World
;; Eat the food and generate a new one.
(define (snake-eat w)
  (define i (add1 (random (sub1 BOARD-WIDTH) r)))
  (define j (add1 (random (sub1 BOARD-HEIGHT) r)))
  (world (snake-grow (world-snake w))
         (posn i j)
         
         #;(posn (- BOARD-WIDTH 1) (- BOARD-HEIGHT 1))))
