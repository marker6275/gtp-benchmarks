#lang racket

(require "mutation-runner.rkt"
         "trace.rkt"
         (submod flow-trace/collapsing compressed trace-api))

;; Desired data:
;; listof
;;   benchmark, precision, trace, blame-label, module, mutation-index
;;
;; MutantTable := (listof MutantOutcome)
;; MutantOutcome := (Bench, PrecisionConfig,
;;                   Trace, Label,
;;                   Module, MutationIndex)
;;
;; Trace := (hash/c Label label-bounds?)
;; MutationIndex := natural?
;; Module := Path
;; Bench := Path
;; Path := string? | path?
;; Label := symbol?
;; PrecisionConfig := (or/c 'none 'types 'max)

(struct mutant-outcome (bench precision
                              outcome
                              blamed
                              mutated
                              distance
                              mutation-index
                              trace)
  #:transparent)

;; Assume have a function that will run a given module with a given
;; mutation with the tracing framework and return a list of labels.
;; trace-of: Path Path syntax? -> Trace
(define (trace-of main-module-path
                  mutated-module-path
                  mutated-module-stx
                  other-modules-to-trace)
  (run-with-tracing main-module-path
                    other-modules-to-trace
                    mutated-module-path
                    mutated-module-stx))

(define (mutant-outcomes/for-modules bench main-module mutatable-modules
                                     #:report-progress [report-progress #f])
  (run-all-mutants/with-modules
   main-module
   mutatable-modules
   #:make-result
   (match-lambda
     [(run-status outcome blamed
                  mutated-module mutated-id
                  precision index)
      (define-values (mutated-stx _) (mutate-module mutated-module index))
      (define trace (trace-of main-module
                              mutated-module
                              mutated-stx
                              mutatable-modules))
      (define distance (trace-distance-between mutated-module blamed trace))
      (define this-mutant-outcome
        (mutant-outcome bench
                        precision
                        outcome
                        blamed
                        distance
                        mutated-id
                        index
                        trace))
      (when report-progress
        (displayln '--------------------)
        (displayln this-mutant-outcome)
        (displayln '--------------------))
      this-mutant-outcome])))

(struct distance (value) #:transparent)
(struct no-blame () #:transparent)
(struct label-missing (label) #:transparent)
#|???
blamed-label85: unbound identifier;
 also, no #%top syntax transformer is bound
  in: blamed-label85
???|#
#;(define/match (trace-distance-between mutated-label blamed-label trace)
  [{(? symbol? mutated)
    (? symbol? blamed-label)
    (hash-table (mutated (label-bounds lower _))
                (blamed-label (label-bounds _ upper))
                _ ...)}
   (distance (- upper lower))]
  [{(? symbol? mutated) (? symbol? blamed) trace}
   (label-missing (if (hash-has-key? trace mutated) blamed mutated))]
  [{_ _ _}
   (no-blame)])
(define (trace-distance-between mutated-label blamed-label trace)
  (cond [(or (false? blamed-label)
             (exn? blamed-label))
         (no-blame)]
        [(and (hash-has-key? trace mutated-label)
              (hash-has-key? trace blamed-label))
         (- (label-bounds-upper (hash-ref trace blamed-label))
            (label-bounds-lower (hash-ref trace mutated-label)))]
        [(hash-has-key? trace mutated-label)
         (label-missing blamed-label)]
        [else
         (label-missing mutated-label)]))

;; temp: Comment out everything but one very short module
(define benchmarks-to-mutate
  '(#;("forth" ("../benchmarks/forth/untyped/main.rkt"
              ("../benchmarks/forth/untyped/command.rkt"
               "../benchmarks/forth/untyped/eval.rkt"
               "../benchmarks/forth/untyped/stack.rkt")))
    #;("snake" ("../benchmarks/snake/untyped/main.rkt"
              ("../benchmarks/snake/untyped/collide.rkt"
               "../benchmarks/snake/untyped/const.rkt"
               "../benchmarks/snake/untyped/cut-tail.rkt"
               "../benchmarks/snake/untyped/data.rkt"
               "../benchmarks/snake/untyped/handlers.rkt"
               "../benchmarks/snake/untyped/motion-help.rkt"
               "../benchmarks/snake/untyped/motion.rkt")))
    ("dungeon" ("../benchmarks/dungeon/untyped/main.rkt"
                ("../benchmarks/dungeon/base/un-types.rkt"
                 "../benchmarks/dungeon/untyped/cell.rkt"
                 "../benchmarks/dungeon/untyped/grid.rkt"
                 "../benchmarks/dungeon/untyped/utils.rkt")))))

#;(define data
  (flatten
   (for/list ([bench (in-list benchmarks-to-mutate)])
     (match-define (list name (list main-module mutatable-modules)) bench)
     (mutant-outcomes/for-modules name main-module mutatable-modules))))
