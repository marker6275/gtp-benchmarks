#lang typed/racket/base

(require
  require-typed-check
)

(require/typed/check "structs.rkt"
  [#:struct Stx ([label : Label])]
  [#:struct (useless-exp Stx) ()]
  [#:struct (Ref useless-exp) ([var : Var])]
  [#:struct (Lam useless-exp) ([formals : (Listof Var)] [call : Exp])]
  [#:struct (Call Stx) ([fun : Exp] [args : (Listof Exp)])]
)

(provide
  (struct-out Stx)
  (struct-out useless-exp)
  (struct-out Ref)
  (struct-out Lam)
  (struct-out Call)
  Exp
  Label
  Var
)

;; =============================================================================

(define-type Exp (U useless-exp Ref Lam Call))
(define-type Label Symbol)
(define-type Var Symbol)

