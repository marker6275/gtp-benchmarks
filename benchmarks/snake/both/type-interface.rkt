#lang typed/racket/base

(require "../../../utilities/require-typed-check-provide.rkt")
(reprovide "data-adaptor.rkt")

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
