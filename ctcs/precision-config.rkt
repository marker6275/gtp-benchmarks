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
         (for-syntax flow-trace/require-introducer))

(provide configurable-ctc
         define/ctc-helper
         define-syntax/ctc-helper)

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
            [current-level-index (index-of levels
                                           current-precision-config)]
            [current-ctc-stx (if (number? current-level-index)
                                 (list-ref ctcs current-level-index)
                                 #'any/c)])
       (begin
         (unless (andmap (curryr member precision-configs) levels)
           (error 'configurable-ctc
                  "Unknown precision config provided at ~a" stx))
         (with-syntax ([ctc-for-current-level current-ctc-stx])
           (syntax/loc current-ctc-stx
             ctc-for-current-level))))]))



(define-syntax-rule (define-ctc-helpers [expansion ctc-helper-id] ...)
  (begin
    (define-syntax (ctc-helper-id stx)
      (syntax-parse stx
        [(_ id/sig . body)
         (with-syntax ([body/escaped (escape-tracing #'body)])
           (syntax/loc stx
             (expansion id/sig . body/escaped)))]))
    ...))

(define-ctc-helpers
  [define define/ctc-helper]
  [define-syntax define-syntax/ctc-helper])
