#lang racket/base

(require "benchmarks.rkt"
         "../mutate/trace-collect.rkt"
         "../mutate/mutation-runner.rkt"
         "../ctcs/current-precision-setting.rkt"
         racket/serialize
         racket/cmdline
         racket/match)

(for ([(bench precision) current-precision-config])
    (unless (equal? precision 'none)
      (eprintf
       "Warning: default precision config has non-none precision ~a for ~a"
       precision
       bench)))

(module+ main
  (define benchmark-name (make-parameter #f))
  (define module-to-mutate (make-parameter #f))
  (define mutation-index (make-parameter #f))
  (define write-modules-to (make-parameter #f))
  (define on-module-exists (make-parameter 'error))

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
   [("-o" "--write-modules-to")
    output-path
    "Path to output mutated modules to"
    (write-modules-to output-path)]
   [("-f" "--overwrite-modules")
    "When writing modules, overwrite files that already exist"
    (on-module-exists 'replace)])

  (define ((invalid-arg fmt-str . fmt-args))
    (eprintf "Invalid argument: ~a\n" (apply format fmt-str fmt-args))
    (exit 1))

  (define bench
    (hash-ref benchmarks (benchmark-name)
              (invalid-arg "benchmark ~a unknown" (benchmark-name))))

  (define module-to-precision-map (deserialize (read)))

  (define result
    (match bench
      [(or (benchmark (and main (== (module-to-mutate))) others)
           (benchmark main (list-no-order (== (module-to-mutate)) others ...)))
       (run-with-mutated-module
        (resolve-bench-path main)
        (resolve-bench-path (module-to-mutate))
        (map resolve-bench-path others)
        (mutation-index)
        module-to-precision-map
        #:timeout/s (* 60 (if (equal? (benchmark-name) "dungeon") 10 5))
        #:modules-base-path (resolve-bench-path (benchmark-name))
        #:write-modules-to (write-modules-to)
        #:on-module-exists (on-module-exists))]
      [_ ((invalid-arg "module-to-mutate ~a is not a module in benchmark ~a"
                       (module-to-mutate)
                       (benchmark-name)))]))

  (define trace-distance-results
    (make-trace-distance-results (benchmark-name) result))

  (write-mutant-outcome/sexp trace-distance-results))
