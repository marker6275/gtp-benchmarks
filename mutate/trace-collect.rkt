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
                  other-modules-to-trace
                  #:suppress-output? [suppress-output? #t])
  (run-with-tracing main-module-path
                    other-modules-to-trace
                    mutated-module-path
                    mutated-module-stx
                    #:suppress-output? suppress-output?))

(define (last-label trace)
  (for/fold ([max-label #f]
             [max-index -1]
             #:result max-label)
            ([(label bounds) (in-hash trace)])
    (define label-index (label-bounds-upper bounds))
    (if (> label-index max-index)
        (values label label-index)
        (values max-label max-index))))

(define (mutant-outcomes/for-modules bench main-module mutatable-modules
                                     #:report-progress [report-progress #f]
                                     #:suppress-output? [suppress-output? #t]
                                     #:memory/gb [memory/gb 3])
  (run-all-mutants/with-modules
   main-module
   mutatable-modules
   #:suppress-output? suppress-output?
   #:memory/gb memory/gb
   #:make-result
   (match-lambda
     [(and run-statuses
           (list (run-status outcomes blames
                             mutated-module* mutated-id*
                             precisions index*)
                 ...))
      (define index (first index*))
      (define mutated-module (first mutated-module*))
      (define mutated-id (first mutated-id*))
      (define-values (mutated-stx _) (mutate-module mutated-module index))
      (define trace (trace-of main-module
                              mutated-module
                              mutated-stx
                              mutatable-modules
                              #:suppress-output? suppress-output?))
      (map (match-lambda
             [(run-status outcome blame _ _ precision _)
              (define blamed-id (if (exn? blame) (last-label trace) blame))
              (define distance (trace-distance-between mutated-id
                                                       blamed-id
                                                       trace))
              (define this-mutant-outcome
                (mutant-outcome bench
                                precision
                                outcome
                                blamed-id
                                mutated-id
                                distance
                                index
                                trace))
              (when report-progress
                (display-mutant-outcome/csv this-mutant-outcome))
              this-mutant-outcome])
           run-statuses)])))

(struct distance (value) #:transparent)
(struct no-blame () #:transparent)
(struct label-missing (label) #:transparent)
(define (trace-distance-between mutated-label blamed-label trace)
  (cond [(false? blamed-label)
         (no-blame)]
        [(and (hash-has-key? trace mutated-label)
              (hash-has-key? trace blamed-label))
         (distance (- (label-bounds-upper (hash-ref trace blamed-label))
                      (label-bounds-lower (hash-ref trace mutated-label))))]
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
    ("snake" ("../benchmarks/snake/untyped/main.rkt"
              ("../benchmarks/snake/untyped/collide.rkt"
               "../benchmarks/snake/untyped/const.rkt"
               "../benchmarks/snake/untyped/cut-tail.rkt"
               "../benchmarks/snake/untyped/data.rkt"
               "../benchmarks/snake/untyped/handlers.rkt"
               "../benchmarks/snake/untyped/motion-help.rkt"
               "../benchmarks/snake/untyped/motion.rkt")))
    #;("dungeon" ("../benchmarks/dungeon/untyped/main.rkt"
                ("../benchmarks/dungeon/base/un-types.rkt"
                 "../benchmarks/dungeon/untyped/cell.rkt"
                 "../benchmarks/dungeon/untyped/grid.rkt"
                 "../benchmarks/dungeon/untyped/utils.rkt")))))


(module+ snake-traces
  ;; Demonstration that traces are different when collected repeatedly
  (define bench "snake")
  (define main-module "../benchmarks/snake/untyped/main.rkt")
  (define mutatable-modules '("../benchmarks/snake/untyped/collide.rkt"
                              "../benchmarks/snake/untyped/const.rkt"
                              "../benchmarks/snake/untyped/cut-tail.rkt"
                              "../benchmarks/snake/untyped/data.rkt"
                              "../benchmarks/snake/untyped/handlers.rkt"
                              "../benchmarks/snake/untyped/motion-help.rkt"
                              "../benchmarks/snake/untyped/motion.rkt"))
  (define mutated-module "../benchmarks/snake/untyped/cut-tail.rkt")
  (define index 1)

  (define-values (mutated-module-stx _)
    (mutate-module mutated-module index))

  (define mutant-run/none (run-with-mutated-module main-module
                                                   mutated-module
                                                   index
                                                   'none))
  (define trace/none (trace-of main-module
                               mutated-module
                               mutated-module-stx
                               mutatable-modules
                               #:suppress-output? #f))
  (define mutant-run/types (run-with-mutated-module main-module
                                                   mutated-module
                                                   index
                                                   'types))
  (define trace/types (trace-of main-module
                                mutated-module
                                mutated-module-stx
                                mutatable-modules))
  (define mutant-run/max (run-with-mutated-module main-module
                                                   mutated-module
                                                   index
                                                   'max))
  (define trace/max (trace-of main-module
                              mutated-module
                              mutated-module-stx
                              mutatable-modules))
  (unless (and (equal? trace/none trace/types)
               (equal? trace/none trace/max))
    (error "Collected traces don't match!")))

(module+ forth-traces
  (define bench "forth")
  (define main-module "../benchmarks/forth/untyped/main.rkt")
  (define mutatable-modules '("../benchmarks/forth/untyped/command.rkt"
                              "../benchmarks/forth/untyped/eval.rkt"
                              "../benchmarks/forth/untyped/stack.rkt"))
  (define mutated-module "../benchmarks/forth/untyped/command.rkt")
  (define index 7)

  (define-values (mutated-module-stx _)
    (mutate-module mutated-module index))

  (define mutant-run/none (run-with-mutated-module main-module
                                                   mutated-module
                                                   index
                                                   'none))
  (define trace/none (trace-of main-module
                               mutated-module
                               mutated-module-stx
                               mutatable-modules
                               #:suppress-output? #f))
  (define mutant-run/types (run-with-mutated-module main-module
                                                   mutated-module
                                                   index
                                                   'types))
  (define mutant-run/max (run-with-mutated-module main-module
                                                   mutated-module
                                                   index
                                                   'max)))

(module+ dungeon-traces
  (define bench "dungeon")
  (define main-module "../benchmarks/dungeon/untyped/main.rkt")
  (define mutatable-modules '("../benchmarks/dungeon/base/un-types.rkt"
                              "../benchmarks/dungeon/untyped/cell.rkt"
                              "../benchmarks/dungeon/untyped/grid.rkt"
                              "../benchmarks/dungeon/untyped/utils.rkt"))
  (define mutated-module "../benchmarks/dungeon/base/un-types.rkt")
  (define index 1)

  (define-values (mutated-module-stx _)
    (mutate-module mutated-module index))

  (define mutant-run/none (run-with-mutated-module main-module
                                                   mutated-module
                                                   index
                                                   'none))
  (define trace/none (trace-of main-module
                               mutated-module
                               mutated-module-stx
                               mutatable-modules
                               #:suppress-output? #f)))

(define/match (display-mutant-outcome/csv outcome)
  [{(mutant-outcome bench
                    precision
                    outcome
                    blamed
                    mutated
                    maybe-distance
                    index
                    _)}
   (define distance/repr (match maybe-distance
                           [(distance n) n]
                           [(no-blame) 'N/A]
                           [(label-missing _) 'M/L]))
   (printf "~a, ~a, ~a, ~a, ~a, ~a, ~a~n"
           bench
           precision
           mutated
           index
           outcome
           blamed
           distance/repr)])

(module+ main
  (define data
    (flatten
     (for/list ([bench (in-list benchmarks-to-mutate)])
       (match-define (list name (list main-module mutatable-modules)) bench)
       ;; with-output-to-file (format "~a.rktd" name) #:exists 'replace
       ;; λ _
       (printf "benchmark, precision, mutated-id, mutant-index, \
outcome, blamed, distance~n")
       (mutant-outcomes/for-modules name main-module mutatable-modules
                                    #:report-progress #t
                                    #:memory/gb 8)))))
