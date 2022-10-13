#lang typed/racket/base

;; Binding environment,
;; helper functions

(require
  "structs-adapted.rkt"
)

(provide
  ;; (struct-out Closure)
  ;; (struct-out Binding)
  empty-benv
  benv-lookup
  benv-extend
  benv-extend*
)

;; =============================================================================

;; -- private

(define-type BEnv (HashTable Var Addr))
(define-type Addr Binding)
(define-type Time (Listof Label))

;; -- structs
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

;; -- public

(: empty-benv BEnv)
(define empty-benv (make-immutable-hasheq '()))

(: benv-lookup (-> BEnv Var Addr))
(define benv-lookup hash-ref)

(: benv-extend (-> BEnv Var Addr BEnv))
(define benv-extend hash-set)

(: benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv))
(define (benv-extend* benv vars addrs)
  (for/fold ([benv benv])
    ([v (in-list vars)]
     [a (in-list addrs)])
    (benv-extend benv v a)))

