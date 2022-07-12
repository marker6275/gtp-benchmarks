#lang racket

(provide require/typed/check/provide
         require/typed/check
         reprovide)

(require syntax/parse/define
         (for-syntax syntax/parse)
         (only-in "require-typed-check-provide.rkt" reprovide))

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
  ;; The un-erased version has this, but it's not necessary since this version doesn't
  ;; have to deal with TR bullshit! We can do the simpler thing instead.
  ;; #:with prov (datum->syntax this-syntax `(provide . ,(attribute clause.provide-stx)) this-syntax)
  #:with prov (datum->syntax this-syntax `(provide (all-from-out ,#'mod-path)) this-syntax)
  (begin req prov))

(define-simple-macro (require/typed/check mod-path clause:r/t/c-clause ...)
  #:with req (datum->syntax this-syntax `(require ,#'mod-path) this-syntax)
  req)

