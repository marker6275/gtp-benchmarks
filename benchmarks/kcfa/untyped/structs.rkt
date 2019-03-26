#lang racket/base

(provide
  (struct-out Stx)
  (struct-out exp)
  (struct-out Ref)
  (struct-out Lam)
  (struct-out Call)
)

(provide Stx-type?
         exp-type?
         Ref-type?
         Lam-type?
         Call-type?
         Stx/c
         exp/c
         Ref/c
         Lam/c
         Call/c)

(require "../../../ctcs/precision-config.rkt"
         racket/contract)

;; =============================================================================

(define/ctc-helper (Stx/c label/c)
  (struct/c Stx label/c))
(define/ctc-helper (exp/c label/c)
  (struct/c exp label/c))
(define/ctc-helper (Ref/c label/c var/c)
  (struct/c Ref label/c
            var/c))
(define/ctc-helper (Lam/c label/c formals/c call/c)
  (struct/c Lam label/c
            formals/c
            call/c))
(define/ctc-helper (Call/c label/c fun/c args/c)
  (struct/c Call label/c
            fun/c
            args/c))
(define/ctc-helper Stx-type? (Stx/c symbol?))
(define/ctc-helper exp-type? (exp/c symbol?))
(define/ctc-helper Ref-type? (Ref/c symbol? symbol?))
(define/ctc-helper Lam-type? (Lam/c symbol?
                                    (listof symbol?)
                                    (or/c exp-type?
                                          Ref-type?
                                          (recursive-contract Lam-type?)
                                          Call-type?)))
(define/ctc-helper Call-type?
  (Call/c symbol?
          (or/c exp-type?
                Ref-type?
                Lam-type?
                (recursive-contract Call-type?))
          (listof (or/c exp-type?
                        Ref-type?
                        Lam-type?
                        (recursive-contract Call-type?)))))


(struct Stx
 (label ;: Symbol]))
))
(struct exp Stx ())

(struct Ref exp
 (var ;: Symbol]))
))
(struct Lam exp
 (formals ;: (Listof Symbol)]
  call ;: (U exp Ref Lam Call)]))
))

(struct Call Stx
 (fun ;: (U exp Ref Lam Call)]
  args ;: (Listof (U exp Ref Lam Call))]))
))
