#lang racket

(provide require/typed/check/provide
         require/typed/check
         reprovide)

(require syntax/parse/define
         require-typed-check
         (only-in "require-typed-check-provide.rkt"
                  reprovide))

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



