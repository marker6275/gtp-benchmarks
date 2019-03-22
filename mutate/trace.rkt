#lang racket

(provide trace-module)


(require syntax/parse
         syntax/strip-context
         "sandbox-runner.rkt")

;; Produce the instrumented syntax for a module to run with flow-trace
(define/contract (trace-module module-file-path module-stx precision-config)
  (path-string?
   syntax?
   (hash/c path-string?
           (hash/c (or/c symbol? path-string?)
                   symbol?))
   . -> .
   syntax?)

  (define config-for-this-module (hash-ref precision-config module-file-path))
  (syntax-parse module-stx
    #:datum-literals [module]
    [(module name lang {~and mod-body (mod-begin body ...)})
     #:with config-stx config-for-this-module
     #:with traced-mod-body
     (datum->syntax #'mod-body
                    (syntax-e #'(mod-begin
                                 #:precision-config config-stx
                                 body ...))
                    #'mod-body
                    #'mod-body)
     (strip-context
      #'(module name (submod flow-trace/collapsing compressed)
          traced-mod-body))]))


(module+ test
  (require ruinit)
  (test-begin
    (ignore
     (define-test (test-stx=? a b)
       (test-equal? (syntax->datum a)
                    (syntax->datum b)))
     (define dummy-mod-path "/tmp/test.rkt")
     (define dummy-mod-stx #'(module mod-name racket (#%module-begin a b c))))
    (test-stx=? (trace-module dummy-mod-path
                              dummy-mod-stx
                              (hash "/tmp/test.rkt" (hash 'a 'none 'b 'max)
                                    "some-other-mod.rkt" (hash 'foo 'types)))
                #'(module mod-name (submod flow-trace/collapsing compressed)
                    (#%module-begin
                     #:precision-config #hash((a . none) (b . max))
                     a b c)))))

(module+ test
  (display-test-results))
