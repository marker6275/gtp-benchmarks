#lang racket

(provide require/typed/check/provide
         require/typed/check
         reprovide
         (rename-out [lax-except-in except-in]))

(require syntax/parse/define
         (for-syntax syntax/parse
                     racket/string
                     racket/function)
         racket/require-syntax
         racket/require
         (only-in "require-typed-check-provide.rkt" reprovide))

(begin-for-syntax
  (define-syntax-class r/t/c-clause
    #:description "require/typed/check clause"
    (pattern [name:id . _]
             #:with provide-stx #'name)
    (pattern [#:opaque name:id . _]
             #:with provide-stx #'name)
    (pattern [#:struct {~or name:id
                            (name:id _)} . _]
             #:with provide-stx (datum->syntax this-syntax
                                               `(struct-out ,(attribute name))
                                               this-syntax))))

(define-simple-macro (require/typed/check/provide mod-path clause:r/t/c-clause ...)
  #:with req (datum->syntax this-syntax `(require (only-in ,#'mod-path ,@(attribute clause.name))) this-syntax)
  ;; The un-erased version has this, but it's not necessary since this version doesn't
  ;; have to deal with TR bullshit! We can do the simpler thing instead.
  ;; #:with prov (datum->syntax this-syntax `(provide . ,(attribute clause.provide-stx)) this-syntax)
  #:with prov (datum->syntax this-syntax `(provide (all-from-out ,#'mod-path)) this-syntax)
  (begin req prov))

(define-simple-macro (require/typed/check mod-path clause:r/t/c-clause ...)
  #:with req (datum->syntax this-syntax `(require ,#'mod-path) this-syntax)
  req)

;; Useful if a require mentions type names - they won't be there in the erased
;; version, and default `except-in` raises a stx error there.
(define-require-syntax (lax-except-in stx)
  (syntax-parse stx
    [(_ subform name:id ...)
     (define names (map (compose1 regexp-quote symbol->string syntax->datum)
                        (attribute name)))
     (define regex (regexp (string-join names "|")))
     (quasisyntax/loc stx
       (subtract-in subform
                    (matching-identifiers-in #,regex subform)))]))
