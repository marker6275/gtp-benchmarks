#lang racket

(provide top-level-defs/by-module)

(require "../utilities/read-module.rkt"
         "../data-collection/benchmarks.rkt"
         syntax/parse)

(define (top-level-defs/by-module bench-name)
  (define bench (hash-ref benchmarks bench-name
                          (λ _ (error 'analyze
                                      "Benchmark ~a not one of known: ~a"
                                      bench-name
                                      (hash-keys benchmarks)))))
  (define mods (map resolve-bench-path
                    (cons (benchmark-main bench)
                          (benchmark-others bench))))
  (for/hash ([mod (in-list mods)])
    (values mod (top-level-defs/in-module mod))))

(define (top-level-defs/in-module path)
  (define mod-stx (read-module path))
  (syntax-parse mod-stx
    #:datum-literals [module define/contract]
    [(module name lang
       (mod-begin
        {~alt {~and def
                    (define/contract _ ...)}
              _:expr} ...))
     (syntax-e #'(def ...))]))
