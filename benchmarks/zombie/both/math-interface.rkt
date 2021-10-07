#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(require/typed/check/provide
 "math.rkt"
 [min (-> Real Real Real)]
 [max (-> Real Real Real)]
 [abs (-> Real Real)]
 [sqr (-> Real Real)]
 [msqrt (-> Real Real)])
