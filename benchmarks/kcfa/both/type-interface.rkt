#lang typed/racket/base

(require "../../../utilities/require-typed-check-provide.rkt")

(require (only-in "structs-adapted.rkt"
                  Exp
                  Label
                  Var))
(provide (all-from-out "structs-adapted.rkt"))

(require/typed/check/provide "ai.rkt"
  (atom-eval (-> BEnv Store (-> Exp Denotable)))
  (next (-> State (Setof State)))
  (explore (-> (Setof State) (Listof State) (Setof State)))
  )

(require/typed/check/provide "structs.rkt"
  [#:struct Stx ([label : Label])]
  [#:struct (exp Stx) ()]
  [#:struct (Ref exp) ([var : Var])]
  [#:struct (Lam exp) ([formals : (Listof Var)] [call : Exp])]
  [#:struct (Call Stx) ([fun : Exp] [args : (Listof Exp)])]
)

(require (only-in "benv-adapted.rkt"
                  BEnv
                  Addr
                  Time))
(provide (all-from-out "benv-adapted.rkt"))
(require/typed/check/provide "benv.rkt"
  [#:struct Closure
    ([lam : Lam]
     [benv : BEnv])]
  [#:struct Binding
    ([var : Var]
     [time : Time])]
  (empty-benv BEnv)
  (benv-lookup (-> BEnv Var Addr))
  (benv-extend (-> BEnv Var Addr BEnv))
  (benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv))
)

(require (only-in "denotable-adapted.rkt"
                  Denotable
                  Store))
(provide (all-from-out "denotable-adapted.rkt"))
(require/typed/check/provide "denotable.rkt"
  [#:struct State
    ([call : Exp]
     [benv : BEnv]
     [store : Store]
     [time : Time])]
   [d-bot Denotable]
   [d-join (-> Denotable Denotable Denotable)]
   [empty-store Store]
   [store-lookup (-> Store Addr Denotable)]
   [store-update (-> Store Addr Denotable Store)]
   [store-update* (-> Store (Listof Addr) (Listof Denotable) Store)]
   [store-join (-> Store Store Store)]
)

(require (only-in "time-adapted.rkt"
                  Value))
(provide (all-from-out "time-adapted.rkt"))
(require/typed/check/provide "time.rkt"
  [time-zero Time]
  [k (Parameterof Natural)]
  [tick (-> Stx Time Time)]
  [alloc (-> Time (-> Var Addr))]
)

