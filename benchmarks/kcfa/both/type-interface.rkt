#lang typed/racket/base

(require "../../../utilities/require-typed-check-provide.rkt")

(reprovide "structs-adapted.rkt"
           "time-adapted.rkt"
           "denotable-adapted.rkt"
           "benv-adapted.rkt")

(require/typed/check/provide "ai.rkt"
  (atom-eval (-> BEnv Store (-> Exp Denotable)))
  (next (-> State (Setof State)))
  (explore (-> (Setof State) (Listof State) (Setof State)))
  )



