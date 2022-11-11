#lang typed/racket

(require require-typed-check)

#;(require/typed/check "data.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])]
  [#:struct snake ([dir : Dir]
                   [segs : NEListof-Posn])]
  [#:struct world ([snake : Snake]
                   [food  : Posn])])

(struct: posn ([x : Real]
               [y : Real])
  #:prefab
  #:type-name Posn)

;;(define-type (NEListof A) (Pairof A (Listof A)))
(define-type NEListof-Posn (Pairof Posn (Listof Posn)))
(define-type Dir (U "up" "down" "left" "right"))
;; (define-type Snake snake)
;; (define-type World world)
;; (define-type Posn  posn)

(provide
 ;; (struct-out posn)
 ;; (struct-out snake)
 ;; (struct-out world)
 Dir
 ;; Snake
 ;; World
 ;; Posn
 NEListof-Posn)
