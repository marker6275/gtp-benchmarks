#lang racket/base

(require (for-syntax syntax/parse
                     racket/list
                     racket/base
                     (only-in racket/function
                              curryr)
                     racket/path
                     racket/runtime-path)
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

(begin-for-syntax
  (define-runtime-path benchmarks-directory "../benchmarks/")
  ; resolve the “..” component in the path, which define-runtime-path doesn’t do
  (define simplified-benchmarks-directory (simplify-path benchmarks-directory)))

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
             (find-relative-path simplified-benchmarks-directory
                                 current-module-path/absolute)]
            [current-module-precision
             (hash-ref current-precision-config
                       (path->string current-module-path/relative)
                       (λ _ (error 'configurable-ctc
                                   "Current module has no configuration: ~v"
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
