#lang typed/racket/base

(require "../../../utilities/require-typed-check-provide.rkt")

(provide NEListof
         Dir)

(define-type (NEListof A) (Pairof A (Listof A)))
(define-type Dir (U "up" "down" "left" "right"))

(struct: snake ([dir  : Dir]
                [segs : (NEListof Posn)])
  #:prefab
  #:type-name Snake)
(struct: world ([snake : Snake]
                [food  : Posn])
  #:prefab
  #:type-name World)

(struct: posn ([x : Real]
               [y : Real])
  #:prefab
  #:type-name Posn)
(provide (struct-out snake)
         Snake
         (struct-out world)
         World
         (struct-out posn)
         Posn)

(require/typed/check/provide "const.rkt"
                             [WORLD (-> World)])
(require/typed/check/provide "motion.rkt"
                             [reset!           (-> Void)]
                             [world->world     (World . -> . World)])

(require/typed/check/provide "collide.rkt"
                             [snake-wall-collide? (Snake . -> . Boolean)]
                             [snake-self-collide? (Snake . -> . Boolean)])
(require/typed/check/provide "motion.rkt"
                             [world-change-dir (World Dir . -> . World)])
