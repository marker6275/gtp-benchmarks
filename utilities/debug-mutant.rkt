#lang racket

(provide debug-mutant)

(require "../data-collection/benchmarks.rkt"
         "../mutate/mutation-runner.rkt"
         (submod "../mutate/mutation-runner.rkt" debug)
         racket/serialize
         racket/runtime-path)

(define-runtime-path gtp-benchmarks "../../gtp-benchmarks")

(define (fixup-paths raw-config-string)
  (regexp-replace*
   "(/projects/p30818|/home/llx9037/proj)/src/gtp-benchmarks([^ ]+)"
   raw-config-string
   (format "\"~a\\2\"" (path->string (simple-form-path gtp-benchmarks)))))

(define (setup-dump-copy-dir! bench-name dump-copy-dir-name)
  (cond [(hash-has-key? benchmarks dump-copy-dir-name)
         (displayln
          (string-append
           "Directory to modules into is an original benchmark, "
           "refusing to overwrite it."))
         #f]
        [else
         (define dump-dir-path (resolve-bench-path dump-copy-dir-name))
         (when (directory-exists? dump-dir-path)
           (delete-directory/files dump-dir-path))
         (copy-directory/files (resolve-bench-path bench-name)
                               dump-dir-path)
         (define compiled-dir-path (build-path dump-dir-path
                                               "untyped/compiled"))
         (when (directory-exists? compiled-dir-path)
           (delete-directory/files compiled-dir-path))
         dump-dir-path]))

(define (debug-mutant bench-name
                      mutated-module
                      index
                      raw-config-string
                      #:print-configs? [print-configs? #t]
                      #:diff-mutant? [diff-mutant? #t]
                      #:run? [run? #f]
                      #:write-modules-to [dump-copy-dir-name #f]
                      #:print-trace? [print-trace? #f])
  (match-define (benchmark main others) (hash-ref benchmarks bench-name))
  (define config-string (fixup-paths raw-config-string))
  (define config (call-with-input-string config-string read))
  (define config/formatted-for-runner (format-raw-config-for-runner config))

  (when print-configs?
    (displayln ",-------------------- Config --------------------")
    (pretty-print config)
    (displayln "`----------------------------------------")
    (newline)
    (displayln ",-------------------- Serialized config --------------------")
    (writeln (serialize config/formatted-for-runner))
    (displayln "`----------------------------------------"))

  (when diff-mutant?
    (displayln ",-------------------- Mutant diff --------------------")
    (diff-mutation (resolve-bench-path mutated-module) index)
    (displayln "`----------------------------------------"))

  (when (and dump-copy-dir-name (not run?))
    (displayln "Warning: modules can only be dumped when #:run? is true."))
  (when run?
    (define dump-path
      (and dump-copy-dir-name
           (setup-dump-copy-dir! bench-name dump-copy-dir-name)))
    (displayln "Running mutant...")
    (match-define
      (run-status trace outcome blamed _ mutated-id _ _)
      (run-with-mutated-module (resolve-bench-path main)
                               (resolve-bench-path mutated-module)
                               (map resolve-bench-path
                                    (set-remove others mutated-module))
                               index
                               config/formatted-for-runner
                               #:timeout/s (* 5 60)
                               #:modules-base-path (resolve-bench-path bench-name)
                               #:write-modules-to dump-path
                               #:on-module-exists 'replace))
    (define blamed-level
      (match blamed
        [(vector id path)
         (hash-ref (hash-ref config/formatted-for-runner path) id)]
        [#f 'no-blamed]))
    (define mutated-id-level
      (hash-ref (hash-ref config/formatted-for-runner
                          (resolve-bench-path mutated-module))
                mutated-id))
    (printf "Run result: ~a ~a
Mutated (~a) is at ~a
Blamed (~a) is at ~a
"
            outcome blamed
            mutated-id mutated-id-level
            blamed blamed-level)

    (when print-trace?
      (printf "~n~nTrace:~n~v" trace))))
