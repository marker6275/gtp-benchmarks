#lang racket

(provide require/typed/check/provide
         require/typed/check
         reprovide)

(require syntax/parse/define
         require-typed-check)

(define-simple-macro (require/typed/check/provide mod-path
                                                  {~and clause
                                                        {~or [name:id . _]
                                                             [#:opaque name:id . _]
                                                             [#:struct {~or struct-name:id
                                                                            (struct-name:id _)} . _]}}
                                                  ...)
  (begin
    (require/typed/check mod-path clause ...)
    (provide {~? name {~@}} ...
             {~? (struct-out struct-name) {~@}} ...)))

(begin-for-syntax
  (require syntax/parse)
  (define-syntax-class reprovide-spec
    (pattern ({~datum except-in} path name:id ...)
             #:with require-spec this-syntax
             #:with provide-spec (attribute path))
    (pattern path:str
             #:with require-spec this-syntax
             #:with provide-spec this-syntax)))

(define-simple-macro (reprovide s:reprovide-spec ...)
  (begin (require s.require-spec ...)
         (provide (all-from-out s.provide-spec) ...)))

