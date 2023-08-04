#lang racket

(require "data.rkt"
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(provide/configurable-contract
 [GRID-SIZE ([max (=/c 30)]
             [types natural?])]
 [BOARD-HEIGHT ([max (=/c 20)]
                [types natural?])]
 [BOARD-WIDTH ([max (=/c 30)]
               [types natural?])]
 [BOARD-HEIGHT-PIXELS ([max (-> (=/c (* GRID-SIZE BOARD-HEIGHT)))]
                       [types (-> natural?)])]
 [BOARD-WIDTH-PIXELS ([max (-> (=/c (* GRID-SIZE BOARD-WIDTH)))]
                      [types (-> natural?)])]
 [SEGMENT-RADIUS ([max (-> (=/c (/ GRID-SIZE 2)))]
                  [types (-> number?)])]
 [FOOD-RADIUS ([max (-> (=/c (/ GRID-SIZE 2)))]
               [types (-> number?)])]
 [WORLD ([max (-> (world/c (snake/c "right"
                                    (snake-segs=?/c (list (posn 5 3))))
                           (posn/c 8 12)))]
         [types (-> world-type?)])])
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


