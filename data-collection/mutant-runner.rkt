#lang racket/base

(require "benchmarks.rkt"
         "../mutate/trace-collect.rkt"
         "../mutate/mutation-runner.rkt"
         "../ctcs/current-precision-setting.rkt"
         racket/serialize
         racket/cmdline
         racket/match
         (only-in racket/hash hash-union))

(for ([(bench precision) current-precision-config])
    (unless (equal? precision 'none)
      (eprintf
       "Warning: default precision config has non-none precision ~a for ~a"
       precision
       bench)))

(define path-to-benchmarks "../benchmarks")
(define (resolve-bench-path p)
  (build-path path-to-benchmarks p))

(module+ main
  (define benchmark-name (make-parameter #f))
  (define module-to-mutate (make-parameter #f))
  (define mutation-index (make-parameter #f))
  (define output-file (make-parameter #f))

  ;; lltodo: this doesn't work actually: basically the job queue
  ;; manager is going to have to specify everything
  ;; - benchmark-name
  ;; - module to mutate
  ;; - mutation index
  ;; - {module -> precision}
  ;; - file to dump the mutant's data
  (command-line
   #:once-each
   [("-b" "--benchmark")
    bench-name
    "Benchmark name"
    (benchmark-name bench-name)]
   [("-m" "--module")
    mod-name
    "Module name"
    (module-to-mutate mod-name)]
   [("-i" "--mutation-index")
    index
    "Mutation index"
    (mutation-index (string->number index))]
   [("-o" "--output")
    outfile
    "Output file"
    (output-file outfile)])

  (define ((invalid-arg fmt-str . fmt-args))
    (printf "Invalid argument: ~a\n" (apply format fmt-str fmt-args))
    (exit 1))

  (define bench
    (hash-ref benchmarks (benchmark-name)
              (invalid-arg "benchmark ~a unknown" (benchmark-name))))

  (displayln "Enter serialized module precision map:")
  (define module-to-precision-map (deserialize (read)))
  #;(define module-to-precision-map/full
    (hash-union module-to-precision-map
                current-precision-config
                #:combine/key (λ (k p _) p)))

  (define result
    (match bench
      [(or (benchmark (and main (== (module-to-mutate))) others)
           (benchmark main (list-no-order (== (module-to-mutate)) others ...)))
       (run-with-mutated-module (resolve-bench-path main)
                                (resolve-bench-path (module-to-mutate))
                                (map resolve-bench-path others)
                                (mutation-index)
                                module-to-precision-map)]
      [_ ((invalid-arg "module-to-mutate ~a is not a module in benchmark ~a"
                       (module-to-mutate)
                       (benchmark-name)))]))

  (displayln result)

  (define trace-distance-results
    (make-trace-distance-results (benchmark-name) result))

  (display-mutant-outcome/csv trace-distance-results))
