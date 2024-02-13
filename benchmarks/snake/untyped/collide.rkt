#lang racket

(require (except-in "data.rkt" posn=?)
         (except-in "const.rkt" WORLD FOOD-RADIUS SEGMENT-RADIUS BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS BOARD-WIDTH BOARD-HEIGHT GRID-SIZE)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         modalc
         "../../curr-mode.rkt")
(require/configurable-contract "const.rkt" WORLD FOOD-RADIUS SEGMENT-RADIUS BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS BOARD-WIDTH BOARD-HEIGHT GRID-SIZE )
(require/configurable-contract "data.rkt" posn=? )

(provide/configurable-contract
 [snake-wall-collide? ([max (modal->i curr-mode ([snk snake-type?])
                                      [result (snk)
                                              (match snk
                                                [(snake _ (cons h _)) (head-collide? h)]
                                                [_ #f])])]
                       [types (curr-mode snake-type? . modal-> . boolean?)])]
 [head-collide? ([max (modal->i curr-mode ([p posn-type?])
                                [result (p)
                                        (not (and (< 0 (posn-x p) BOARD-WIDTH)
                                                  (< 0 (posn-y p) BOARD-HEIGHT)))])]
                 [types (curr-mode posn-type? . modal-> . boolean?)])]
 [snake-self-collide? ([max (modal->i curr-mode ([snk snake-type?])
                                      [result (snk)
                                              (match snk
                                                [(snake _ (cons h t))
                                                 (memf? (posn=?/c h) t)]
                                                [_ #f])])]
                       [types (curr-mode snake-type? . modal-> . boolean?)])]
 [segs-self-collide? ([max (modal->i curr-mode ([h posn-type?]
                                                [segs snake-segs?])
                                     [result (h segs)
                                             (if (empty? segs)
                                                 #f
                                                 (memf? (posn=?/c h) segs))])]
                      [types (curr-mode posn-type? snake-segs? . modal-> . boolean?)])])

;; (provide
;; snake-wall-collide?
;; snake-self-collide?)


;; snake-wall-collide? : Snake -> Boolean
;; Is the snake colliding with any of the walls?
(define (snake-wall-collide? snk)
  (head-collide? (car (snake-segs snk))))

;; head-collide? : Posn -> Boolean
(define (head-collide? p)
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

(define/ctc-helper (truthy->bool x) (if x #t #f))
(define/ctc-helper memf? (compose truthy->bool memf))

;; snake-self-collide? : Snake -> Boolean
(define (snake-self-collide? snk)
  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))

;; segs-self-collide? : Posn Segs -> Boolean
(define (segs-self-collide? h segs)
  (cond [(empty? segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))
