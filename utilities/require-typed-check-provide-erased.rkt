#lang racket

(provide require/typed/check/provide
         require/typed/check
         reprovide)

(require syntax/parse/define
         (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class r/t/c-clause
    #:description "require/typed/check clause"
    (pattern [name:id . _]
             #:with provide-stx #'name)
    (pattern [#:opaque name:id . _]
             #:with provide-stx #'name)
    (pattern [#:struct {~or struct-name:id
                            (struct-name:id _)} . _]
             #:with provide-stx (datum->syntax this-syntax
                                               `(struct-out ,(attribute struct-name))
                                               this-syntax))))

(define-simple-macro (require/typed/check/provide mod-path clause:r/t/c-clause ...)
  #:with req (datum->syntax this-syntax `(require ,#'mod-path) this-syntax)
  #:with prov (datum->syntax this-syntax `(provide . ,(attribute clause.provide-stx)) this-syntax)
  (begin req prov))

(define-simple-macro (require/typed/check mod-path clause:r/t/c-clause ...)
  #:with req (datum->syntax this-syntax `(require ,#'mod-path) this-syntax)
  req)

(define-simple-macro (reprovide mod-path ...)
  (begin (require mod-path ...)
         (provide (all-from-out mod-path) ...)))
