#lang typed/racket/base

;; (require
  ;; require-typed-check
;; )

;; (require/typed/check "structs.rkt"
;;   [#:struct Stx ([label : Label])]
;;   [#:struct (exp Stx) ()]
;;   [#:struct (Ref exp) ([var : Var])]
;;   [#:struct (Lam exp) ([formals : (Listof Var)] [call : Exp])]
;;   [#:struct (Call Stx) ([fun : Exp] [args : (Listof Exp)])]
;; )

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

(provide
  ;; (struct-out Stx)
  ;; (struct-out exp)
  ;; (struct-out Ref)
  ;; (struct-out Lam)
  ;; (struct-out Call)
  Exp
  Label
  Var
)

;; =============================================================================

(define-type Exp (U exp Ref Lam Call))
(define-type Label Symbol)
(define-type Var Symbol)

