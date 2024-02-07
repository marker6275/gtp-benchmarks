#lang racket

(require (only-in "data.rkt"
                  posn
                  snake
                  world
                  posn?
                  posn-type?
                  posn/c
                  snake-segs?
                  snake-segs=?/c
                  snake-dir?
                  snake-type?
                  snake/c
                  snake=?/c
                  posn=?/c
                  world/c
                  world-type?
                  world=?/c
                  food=?/c)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         modalc
         "../../curr-mode.rkt")
(require/configurable-contract "data.rkt"
                               posn=?)

(provide/configurable-contract
 [GRID-SIZE ([max (modal/c curr-mode (=/c 30))]
             [types (modal/c curr-mode natural?)])]
 [BOARD-HEIGHT ([max (modal/c curr-mode (=/c 20))]
                [types (modal/c curr-mode natural?)])]
 [BOARD-WIDTH ([max (modal/c curr-mode (=/c 30))]
               [types (modal/c curr-mode natural?)])]
 [BOARD-HEIGHT-PIXELS ([max (modal/c curr-mode (=/c (* GRID-SIZE BOARD-HEIGHT)))]
                       [types (modal/c curr-mode natural?)])]
 [BOARD-WIDTH-PIXELS ([max (modal/c curr-mode (=/c (* GRID-SIZE BOARD-WIDTH)))]
                      [types (modal/c curr-mode natural?)])]
 [SEGMENT-RADIUS ([max (modal/c curr-mode (=/c (/ GRID-SIZE 2)))]
                  [types (modal/c curr-mode number?)])]
 [FOOD-RADIUS ([max (modal/c curr-mode (=/c (/ GRID-SIZE 2)))]
               [types (modal/c curr-mode number?)])]
 [WORLD ([max (modal/c curr-mode (world/c (snake/c "right"
                                    (snake-segs=?/c (list (posn 5 3))))
                           (posn/c 8 12)))]
         [types (modal/c curr-mode world-type?)])])
;; (provide
;;  WORLD
;;  GRID-SIZE
;;  BOARD-HEIGHT-PIXELS
;;  BOARD-WIDTH
;;  BOARD-HEIGHT)

(define GRID-SIZE
  30)
(define BOARD-HEIGHT
  20)
(define BOARD-WIDTH
  30)
(define (BOARD-HEIGHT-PIXELS)
  (* GRID-SIZE BOARD-HEIGHT))
(define (BOARD-WIDTH-PIXELS)
  (* GRID-SIZE BOARD-WIDTH))
(define (SEGMENT-RADIUS)
  (/ GRID-SIZE 2))
(define (FOOD-RADIUS)
  (SEGMENT-RADIUS))
(define (WORLD)
  (world (snake "right" (cons (posn 5 3) empty))
         (posn 8 12)))


