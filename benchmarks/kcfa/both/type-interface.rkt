#lang typed/racket/base

(require "../../../utilities/require-typed-check-provide.rkt")

(reprovide "structs-adapted.rkt"
           "time-adapted.rkt"
           "denotable-adapted.rkt"
           "benv-adapted.rkt")

(struct Stx
  ([label : Symbol])
  #:prefab)

(struct exp Stx ()
  #:prefab)

(struct Ref exp
 ([var : Symbol])
  #:prefab)

(struct Lam exp
 ([formals : (Listof Symbol)]
  [call : (U exp Ref Lam Call)])
  #:prefab)

(struct Call Stx
 ([fun : (U exp Ref Lam Call)]
  [args : (Listof (U exp Ref Lam Call))])
  #:prefab)

(struct Closure
 ([lam : Lam]
  [benv : BEnv])
  #:prefab)

(struct Binding
 ([var : Var]
  [time : Time])
  #:prefab)

(define-type Exp (U exp Ref Lam Call))

(struct State
 ([call : Exp]
  [benv : BEnv]
  [store : Store]
  [time : Time])
  #:prefab)

(provide
 (struct-out Stx)
 (struct-out exp)
 (struct-out Ref)
 (struct-out Lam)
 (struct-out Call)
 (struct-out Closure)
 (struct-out Binding)
 (struct-out State)
 Exp)

(require/typed/check/provide "ai.rkt"
  (atom-eval (-> BEnv Store (-> Exp Denotable)))
  (next (-> State (Setof State)))
  (explore (-> (Setof State) (Listof State) (Setof State)))
  )



