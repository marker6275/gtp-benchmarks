#lang typed/racket/base

(require
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


;; ---

(provide
  time-zero
  k
  tick
  alloc
)

;; =============================================================================

;; ---
(define-type Value Closure)

(: take* (All (A) (-> (Listof A) Natural (Listof A))))
(define (take* l n)
  (for/list ([e (in-list l)]
             [i (in-range n)])
    e))

;; ---

(: time-zero Time)
(define time-zero '())

(: k (Parameterof Natural))
(define k (make-parameter 1))

(: tick (-> Stx Time Time))
(define (tick call time)
  (define label (Stx-label call))
  (take* (cons label time) (k)))

(: alloc (-> Time (-> Var Addr)))
(define ((alloc time) var)
  (Binding var time))

