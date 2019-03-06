#lang racket

(provide trace-module)


(require syntax/parse
         syntax/strip-context
         "sandbox-runner.rkt")

;; Produce the instrumented syntax for a module to run with flow-trace
(define/contract (trace-module module-file-path module-stx)
  (path-string? syntax? . -> . syntax?)

  (syntax-parse module-stx
    #:datum-literals [module #%module-begin]
    [(module name lang (#%module-begin body ...))
     (strip-context
      #`(module name (submod flow-trace/collapsing compressed)
          (#%module-begin body ...)))]))


(module+ test
  (require ruinit)
  (test-begin
    (ignore
     (define-test (test-stx=? a b)
       (test-equal? (syntax->datum a)
                    (syntax->datum b)))
     (define dummy-mod-path (string->path "/tmp/test.rkt"))
     (define dummy-mod-stx #'(module mod-name racket (#%module-begin a b c))))
    (test-stx=? (trace-module dummy-mod-path
                              dummy-mod-stx)
                #'(module mod-name (submod flow-trace/collapsing compressed)
                    (#%module-begin a b c)))
    #;(test-stx=? (trace-module (string->path "c.rkt") )
                  #'(module c (submod flow-trace/collapsing compressed)
                      (#%module-begin (void))))))

(module+ test
  (display-test-results))
