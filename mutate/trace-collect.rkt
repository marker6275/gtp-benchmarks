#lang racket

(require "mutation-runner.rkt")

;; Desired data:
;; listof
;;   benchmark, precision, trace, blame-label, module, mutation-index
;;
;; MutantTable := (listof MutantOutcome)
;; MutantOutcome := (Bench, PrecisionConfig,
;;                   Trace, Label,
;;                   Module, MutationIndex)
;;
;; Trace := (listof Label)
;; MutationIndex := natural?
;; Module := Path
;; Bench := Path
;; Path := string? | path?
;; Label := symbol?
;; PrecisionConfig := (or/c 'none 'types 'max)

(struct mutant-outcome (bench precision
                              trace outcome blamed
                              mutated mutation-index)
  #:transparent)

;; Assume have a function that will run a given module with a given
;; mutation with the tracing framework and return a list of labels.
;; trace-of: Path Path syntax? -> Trace
(define (trace-of main-module-path mutated-module-path mutated-module-stx)
  '(a b c))

(define (mutant-outcomes/for-modules bench main-module mutatable-modules)
  (run-all-mutants/with-modules
   main-module
   mutatable-modules
   #:make-result
   (match-lambda
     [(run-status outcome blamed
                  mutated-module mutated-id
                  precision index)
      (define-values (mutated-stx _) (mutate-module mutated-module index))
      (mutant-outcome bench
                      precision
                      (trace-of main-module
                                mutated-module
                                mutated-stx)
                      outcome
                      blamed
                      mutated-id
                      index)])))

;; temp: Comment out everything but one very short module
(define benchmarks-to-mutate
  '(#;("forth" ("../benchmarks/forth/untyped/main.rkt"
              ("../benchmarks/forth/untyped/command.rkt"
               "../benchmarks/forth/untyped/eval.rkt"
               "../benchmarks/forth/untyped/stack.rkt")))
    ("snake" ("../benchmarks/snake/untyped/main.rkt"
              (#;"../benchmarks/snake/untyped/collide.rkt"
               #;"../benchmarks/snake/untyped/const.rkt"
               "../benchmarks/snake/untyped/cut-tail.rkt"
               #;"../benchmarks/snake/untyped/data.rkt"
               #;"../benchmarks/snake/untyped/handlers.rkt"
               #;"../benchmarks/snake/untyped/motion-help.rkt"
               #;"../benchmarks/snake/untyped/motion.rkt")))
    #;("dungeon" ("../benchmarks/dungeon/untyped/main.rkt"
                ("../benchmarks/dungeon/base/un-types.rkt"
                 "../benchmarks/dungeon/untyped/cell.rkt"
                 "../benchmarks/dungeon/untyped/grid.rkt"
                 "../benchmarks/dungeon/untyped/utils.rkt")))))

#;(define data
  (flatten
   (for/list ([bench (in-list benchmarks-to-mutate)])
     (match-define (list name (list main-module mutatable-modules)) bench)
     (mutant-outcomes/for-modules name main-module mutatable-modules))))
