#lang typed/racket/base

(require
  require-typed-check
  "structs-adapted.rkt"
  "benv-adapted.rkt"
)

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


(require/typed/check "time.rkt"
  [time-zero Time]
  [k (Parameterof Natural)]
  [tick (-> Stx Time Time)]
  [alloc (-> Time (-> Var Addr))]
)

;; ---

(provide
  time-zero
  k
  tick
  alloc
  Value
)

;; =============================================================================

;; -- time.rkt
(define-type Value Closure)

