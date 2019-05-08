#lang at-exp racket

(provide debug-mutant
         read/fixup-config)

(require "../data-collection/benchmarks.rkt"
         "../mutate/mutation-runner.rkt"
         "../mutate/trace-collect.rkt"
         (submod "../mutate/mutation-runner.rkt" debug)
         racket/serialize
         racket/runtime-path
         (submod flow-trace/collapsing compressed trace-api))

(define-runtime-path gtp-benchmarks "..")

;; Note: assumes paths have *no* spaces in them
(define (fixup-paths raw-config-string)
  (define raw-config-string/no-paths
    (regexp-replace* "#<path:([^>]+)>" raw-config-string "\\1"))
  (define path-replacement
    (format "~a\\2" (path->string (simple-form-path gtp-benchmarks))))
  (define config-string/local-paths
    (regexp-replace*
     "(/projects/p30818|/home/llx9037/proj)/src/gtp-benchmarks([^ ]+)"
     raw-config-string/no-paths
     path-replacement))
  ;; wrap paths in quotes if they don't already have them
  (define config-string/local-paths/quoted
    (if (string-contains? raw-config-string "\"/")
         config-string/local-paths
         (regexp-replace* #px"(/[^\\s]+)"
                          config-string/local-paths
                          "\"\\1\"")))
  config-string/local-paths/quoted)

(define (setup-dump-copy-dir! bench-name dump-copy-dir-name)
  (cond [(hash-has-key? benchmarks dump-copy-dir-name)
         (displayln @~a{
                        Directory to modules into is an original benchmark,
                        refusing to overwrite it.
                        })
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

(define (read/fixup-config raw-config-string)
  (define config-string (fixup-paths raw-config-string))
  (call-with-input-string config-string read))

(define (debug-mutant bench-name
                      mutated-module
                      index
                      raw-config-string
                      #:print-configs? [print-configs? #f]
                      #:diff-mutant? [diff-mutant? #f]
                      #:run? [run? #f]
                      #:write-modules-to [dump-copy-dir-name #f]
                      #:print-trace? [print-trace? #f]
                      #:suppress-output? [suppress-output? #t]
                      #:no-mutate? [no-mutate? #f]
                      #:distance-analyze? [analyze? #f])
  (match-define (benchmark main others) (hash-ref benchmarks bench-name))
  (define config (read/fixup-config raw-config-string))
  (define config/formatted-for-runner (format-raw-config-for-runner config))

  (when print-configs?
    (displayln @~a{
                   ,-------------------- Config --------------------
                   @(pretty-format config)
                   `------------------------------------------------

                   ,--------------- Serialized config --------------
                   @~s[(serialize config/formatted-for-runner)]
                   `------------------------------------------------
                   }))

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
    (match-define (and (run-status trace outcome blamed _ mutated-id _ _)
                       result)
      (run-with-mutated-module
       (resolve-bench-path main)
       (resolve-bench-path mutated-module)
       (map resolve-bench-path
            (set-remove others mutated-module))
       index
       config/formatted-for-runner
       #:timeout/s (* 5 60)
       #:modules-base-path (resolve-bench-path bench-name)
       #:write-modules-to dump-path
       #:on-module-exists 'replace
       #:suppress-output? suppress-output?
       #:mutator
       (cond [no-mutate?
              (displayln "Running original benchmark with configuration...")
              (λ (stx _) (values stx '<none>))]
             [else
              (displayln "Running mutant...")
              mutate-module])))
    (define maybe-blamed-vec
      (match blamed
        [(or #f (? exn?)) 'no-blamed]
        [(cons (? vector? vec) marks) vec]
        [(? cons? mod-path)
         (displayln
          @~a{
              Blamed is module path: @|mod-path|,
              likely a ctc violation in flow-trace
              })
         'no-blamed]
        [else (displayln @~a{Blamed has unexpected shape: @blamed})]))
    (define blamed-level
      (match maybe-blamed-vec
        [(vector id path)
         (hash-ref (hash-ref config/formatted-for-runner path) id)]
        [_ 'no-blame]))
    (define mutated-id-level
      (hash-ref (hash-ref config/formatted-for-runner
                          (resolve-bench-path mutated-module))
                mutated-id))
    (displayln @~a{
                   Run result: @outcome @maybe-blamed-vec

                   Mutated (@mutated-id) is at @mutated-id-level

                   Blamed (@maybe-blamed-vec) is at @blamed-level

                   Trace length: @(trace-length trace)

                   Distance: @(if analyze?
                                  (struct-copy
                                   mutant-run
                                   (make-trace-distance-results bench-name
                                                                result)
                                   [precision "[omitted]"]
                                   [trace "[omitted]"])
                                  "Disabled")
                   })

    (when (exn? blamed)
      (displayln ",-------------------- Exn message --------------------")
      ((error-display-handler) (exn-message blamed) blamed)
      (displayln "`--------------------------------------------------"))

    (when print-trace?
      (displayln @~a{

                     Trace:
                     @~v[trace]
                     }))))
