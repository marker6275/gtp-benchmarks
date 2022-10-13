#lang typed/racket/base

(require
  require-typed-check
  "structs-adapted.rkt"
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

(require/typed/check "benv.rkt"
  ;; [#:struct Closure
  ;;   ([lam : Lam]
  ;;    [benv : BEnv])]
  ;; [#:struct Binding
  ;;   ([var : Var]
  ;;    [time : Time])]
  (empty-benv BEnv)
  (benv-lookup (-> BEnv Var Addr))
  (benv-extend (-> BEnv Var Addr BEnv))
  (benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv))
)
(provide
  ;; (struct-out Closure)
  ;; (struct-out Binding)
  empty-benv
  benv-lookup
  benv-extend
  benv-extend*
  BEnv
  Addr
  Time
)

;; =============================================================================

(define-type BEnv (HashTable Var Addr))
(define-type Addr Binding)
(define-type Time (Listof Label))
