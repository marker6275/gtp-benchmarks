#lang racket/base

(require (for-syntax syntax/parse
                     racket/list
                     racket/base
                     (only-in racket/function
                              curryr))
         racket/contract
         (only-in racket/function curry)
         (only-in racket/match match)
         (for-syntax "current-precision-setting.rkt")
         (only-in (submod flow-trace/collapsing compressed)
                  define/ctc-helper
                  define-syntax/ctc-helper
                  define-syntax-rule/ctc-helper))

(provide configurable-ctc
         (all-from-out (submod flow-trace/collapsing compressed)))

(define-for-syntax benchmarks-path "../../gtp-benchmarks/benchmarks")

;; Usage:
;; (configurable-ctc [<unquoted-precision-config> contract?] ...)
;;
;; No need to specify the 'none case: any unspecified cases will
;; become any/c when that configuration is used.
;;
;; Example:
;; (define/contract (f x)
;;   (configurable-ctc [max (-> number? string?)]
;;                     [types procedure?]))
(define-syntax (configurable-ctc stx)
  (syntax-parse stx
    [(_ [level ctc] ...)
     (let* ([levels (syntax->datum #'(level ...))]
            [ctcs (flatten (syntax->list #'(ctc ...)))]
            [current-module-path/absolute (path->string (syntax-source stx))]
            [current-module-path/relative
             (second
              (regexp-match #rx"^.*/gtp-benchmarks/benchmarks/(.+)"
                            current-module-path/absolute))]
            [current-module-precision
             (hash-ref current-precision-config
                       current-module-path/relative
                       (λ _ (error 'configurable-ctc
                                   "Current module has no configuration: ~a"
                                   current-module-path/relative)))]
            [current-level-index (index-of levels
                                           current-module-precision)]
            [current-ctc-stx
             (cond [(number? current-level-index)
                    (list-ref ctcs current-level-index)]
                   ;; none wasn't really intended to be specified
                   [(equal? current-module-precision 'none)
                    #'any/c]
                   [else
                    (error 'configurable-ctc
                           "Contract fails to specify current module precision: ~a"
                           current-module-precision)])])
       (begin
         (unless (andmap (curryr member precision-configs) levels)
           (error 'configurable-ctc
                  "Unknown precision config provided at ~a" stx))
         (with-syntax ([ctc-for-current-level current-ctc-stx])
           (syntax/loc current-ctc-stx
             ctc-for-current-level))))]))
