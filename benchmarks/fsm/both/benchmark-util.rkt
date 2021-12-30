#lang racket

(provide
  require/typed/check
  require/typed/check/provide
)

(require
  (for-syntax
    typed/untyped-utils
    syntax/parse)
  (only-in typed/racket require/typed)
  (prefix-in typed: (only-in typed/racket require))
  syntax/parse/define
)

;; =============================================================================

(define-syntax (require/typed/check stx)
  (syntax-parse stx 
    #:literals (prefix-in)
    [(_ (prefix-in p m:str) rt-clause ...)
     (cond 
       [(not (syntax-local-typed-context?))
        #'(require (prefix-in p m))]
       [(and (syntax-local-typed-context?)
             (module->language-info (syntax->datum #'m) #t))
        #'(typed:require (prefix-in p m))]
       [else 
        #'(require/typed m rt-clause ...)])]
    [(_ m:str rt-clause ...)
     (cond 
       [(not (syntax-local-typed-context?))
        #'(require m)]
       [(and (syntax-local-typed-context?)
             (module->language-info (syntax->datum #'m) #t))
        #'(typed:require m)]
       [else 
        #'(require/typed m rt-clause ...)])]))

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
