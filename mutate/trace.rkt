#lang racket

(provide trace-module
         maybe-path->string make-safe-for-reading)


(require syntax/parse
         syntax/strip-context
         "sandbox-runner.rkt")

(define (maybe-path->string mp)
  (if (path? mp) (path->string mp) mp))
(define make-safe-for-reading
  (match-lambda
    [(? path? p)
     (path->string p)]
    [(? hash? h)
     (for/hash ([(k v) (in-hash h)])
       (values (make-safe-for-reading k)
               (make-safe-for-reading v)))]
    [(and other (or (? symbol?) (? string?)))
     other]))

;; Produce the instrumented syntax for a module to run with flow-trace
(define/contract (trace-module module-file-path module-stx precision-config)
  (path?
   syntax?
   (hash/c path?
           (hash/c (or/c symbol? path?)
                   symbol?))
   . -> .
   syntax?)

  (define mod-path/simplified module-file-path)
  (define config-for-this-module
    (hash-ref precision-config mod-path/simplified
              (λ _
                (error 'trace-module
                       "Could not find module ~v in config ~v."
                       mod-path/simplified
                       precision-config))))
  (syntax-parse module-stx
    #:datum-literals [module]
    [(module name lang {~and mod-body (mod-begin body ...)})
     #:with config-stx (make-safe-for-reading config-for-this-module)
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
     (define dummy-mod-path (string->path "/tmp/test.rkt"))
     (define dummy-mod-stx #'(module mod-name racket (#%module-begin a b c))))
    (test-stx=? (trace-module dummy-mod-path
                              dummy-mod-stx
                              (hash (string->path "/tmp/test.rkt") (hash 'a 'none 'b 'max)
                                    (string->path "some-other-mod.rkt") (hash 'foo 'types)))
                #'(module mod-name (submod flow-trace/collapsing compressed)
                    (#%module-begin
                     #:precision-config #hash((a . none) (b . max))
                     a b c)))))

(module+ test
  (display-test-results))
