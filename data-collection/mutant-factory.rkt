#lang racket/base

(require "benchmarks.rkt"
         "../ctcs/current-precision-setting.rkt"
         "../mutate/mutation-runner.rkt"
         "../mutate/instrumented-module-runner.rkt"
         "../mutate/trace-collect.rkt"
         racket/file
         racket/format
         racket/match
         racket/system
         racket/serialize
         racket/set
         racket/port
         racket/file
         racket/logging
         racket/date
         racket/runtime-path
         racket/list
         racket/random)

(module+ test
  (provide (struct-out mutant)
           (struct-out mutant-process)
           (struct-out dead-mutant-process)
           (struct-out aggregate-mutant-result)
           (struct-out bench-info)
           (struct-out factory)
           process-limit
           data-output-dir
           benchmarks-dir-path

           run-all-mutants*configs
           spawn-mutants/of-module
           maybe-spawn-configured-mutants
           spawn-mutants/precision-sampling
           add-mutant-sample
           spawn-mutant/following-blame
           spawn-mutant
           sweep-dead-mutants
           process-dead-mutant
           add-mutant-result
           max-mutation-index-exceeded?
           append-mutant-result!
           get-mutant-result
           blame-outcome?
           try-get-blamed
           config-at-max-precision-for?
           increment-config-precision-for
           make-max-bench-config))


(define N-SAMPLES 10)
(define MAX-CONFIG 'max)



(define-runtime-path mutant-runner-path "mutant-runner.rkt")
(define-runtime-path benchmarks-dir-path "../benchmarks/")
(define racket-path (find-executable-path (find-system-path 'exec-file)))

(define process-limit (make-parameter 3))
(define data-output-dir (make-parameter "./mutant-data"))
(define mutant-error-log (make-parameter "./mutant-errors.txt"))

(define-logger factory)
(define-syntax-rule (log-factory level msg v ...)
  (when (log-level? factory-logger 'level)
    (log-message factory-logger
                 'level
                 (format (string-append "[~a] " msg)
                         (date->string (current-date) #t)
                         v ...))))
(define (failure-msg m)
  (string-append "***** ERROR *****\n" m "\n**********"))

(define (spawn-mutant-runner benchmark-name
                             module-to-mutate
                             mutation-index
                             config-hash
                             outfile)
  (call-with-output-file outfile #:mode 'text
    (λ (outfile-port)
      (call-with-output-file (mutant-error-log) #:mode 'text #:exists 'append
        (λ (error-log-port)
          (match-define (list #f runner-in _ #f runner-ctl)
            (process*/ports outfile-port #f error-log-port
                            racket-path "--"
                            mutant-runner-path
                            "-b" benchmark-name
                            "-m" module-to-mutate
                            "-i" (~a mutation-index)))
          (write (serialize config-hash) runner-in)
          (close-output-port runner-in)
          runner-ctl)))))



;; module: path-string?
;; index:  natural?
(struct mutant (module index) #:transparent)

;; will? := (factory? dead-mutant-process? -> factory?)
;; result? := list? (see `trace-collect.rkt`)
;; ctc-level? := symbol?
;; config? := (hash path-string? (hash (or symbol? path-string?) ctc-level?))

;; mutant: mutant?
;; config: config?
;; file:   path-string?
;; ctl:    process-ctl-fn? (see `process` docs)
;; will:   will?
;;     A transformation of the factory to be performed upon death of the mutant
(struct mutant-process (mutant config file ctl will id) #:transparent)
;; result: result?
(struct dead-mutant-process (mutant config result id) #:transparent)
(struct aggregate-mutant-result (mutant file) #:transparent)

;; mutant-results? := (hash mutant? aggregate-mutant-result?)

;; name:       string?
;; max-config: config?
(struct bench-info (name max-config) #:transparent)

;; bench: bench-info?
;; results: mutant-results?
;;     The map from mutant to completed processes (which contain a data file).
;; active-mutants: (set mutant-process?)
;;     The set of actively running mutant processes.
;; active-mutant-count: natural?
;;     The size of `active-mutants`.
;;     INVARIANT: (= active-mutant-count (set-count active-mutants))
;; mutant-samples: mutant? |-> (set config?)
;;     The set of precision config samples checked for each mutant.
;; total-mutants-spawned: natural?
;;     Count of the total number of mutants spawned by this factory.
;;     This is primarily useful to making every new mutant file unique.
(struct factory (bench
                 results
                 active-mutants
                 active-mutant-count
                 mutant-samples
                 total-mutants-spawned)
  #:transparent)

;; string?
;; path-string?
;; ->
;; mutant-results?
(define (run-all-mutants*configs bench-name)
  (define bench (lookup-benchmark-with-name bench-name))
  (define mutatable-modules (cons (benchmark-main bench)
                                  (benchmark-others bench)))
  (define max-config (make-max-bench-config bench))
  (log-factory info "Running mutants of benchmark ~a, which has modules:~n~a"
                    bench-name
                    mutatable-modules)

  (unless (directory-exists? (data-output-dir))
    (log-factory debug "Creating output directory ~a." (data-output-dir))
    (make-directory (data-output-dir)))

  (define factory-state
    (for/fold ([factory-state (factory (bench-info bench-name max-config)
                                       (hash) (set) 0 (hash) 0)])
              ([module-to-mutate mutatable-modules])

      (log-factory info "Processing mutants of module ~a."
                        module-to-mutate)
      (spawn-mutants/of-module factory-state module-to-mutate)))

  (log-factory info "Finished spawning all mutant runners.")
  (babysit-mutants factory-state))

;; factory?
;; path-string?
;; ->
;; factory?
(define (spawn-mutants/of-module the-factory module-to-mutate)
  ;; This must be an unbounded loop, number of mutants unknown
  (let next-mutant ([mutation-index 0]
                    [current-factory the-factory])
    (cond [(max-mutation-index-exceeded? module-to-mutate mutation-index)
           current-factory]

          [else
           (next-mutant
            (add1 mutation-index)
            (maybe-spawn-configured-mutants current-factory
                                            (mutant module-to-mutate
                                                    mutation-index)))])))

;; factory? mutant? -> factory?
;; Spawns a test mutant and if that mutant has a blame result at
;; max contract configuration, then samples the precision lattice
;; and spawns mutants for each samples point
;; Note that sampling the precision lattice is done indirectly by
;; just generating random configs
(define (maybe-spawn-configured-mutants the-factory mutant-program)
  (match-define (mutant module-to-mutate mutation-index) mutant-program)
  (log-factory debug
               "  Trying to spawn test mutant for ~a @ ~a."
               module-to-mutate
               mutation-index)
  (define max-config (bench-info-max-config (factory-bench the-factory)))
  (spawn-mutant the-factory
                module-to-mutate
                mutation-index
                max-config
                ;; mutant will
                (λ (current-factory dead-proc)
                  (cond
                    [(blame-outcome? dead-proc)
                     (log-factory info
                                  "  Mutant ~a @ ~a has blame. Sampling..."
                                  module-to-mutate
                                  mutation-index)
                     (spawn-mutants/precision-sampling current-factory
                                                       mutant-program)]
                    [else
                     (log-factory info
                                  "  Mutant ~a @ ~a has no blame; discarding."
                                  module-to-mutate
                                  mutation-index)
                     current-factory]))))

;; path-string? mutant? -> factory?
(define (spawn-mutants/precision-sampling the-factory mutant-program)
  (define max-config (bench-info-max-config (factory-bench the-factory)))
  (define samples (sample-config max-config N-SAMPLES))
  (define (resample a-factory)
    (define sample (set-first (sample-config max-config 1)))
    (define samples-seen (hash-ref (factory-mutant-samples a-factory)
                                   mutant-program))
    (cond
      ;; ll: We're sampling *with* replacement; uncomment for *without*
      #;[(set-member? samples-seen sample)
         (resample a-factory)]
      [else
       (values sample
               (add-mutant-sample a-factory mutant-program sample))]))
  (define new-factory
    (for/fold ([current-factory the-factory])
              ([sampled-config (in-set samples)])
      (define factory+sample
        (add-mutant-sample current-factory mutant-program sampled-config))
      (try-spawn-blame-following-mutant factory+sample
                                        mutant-program
                                        sampled-config
                                        resample)))
  (log-factory info
               "    Completed sampling for mutant ~a @ ~a."
               (mutant-module mutant-program)
               (mutant-index mutant-program))
  new-factory)

(define (add-mutant-sample the-factory mutant-program new-sample)
  (define mutant-samples (factory-mutant-samples the-factory))
  (define samples-for-mutant (hash-ref mutant-samples mutant-program
                                       (λ _ (set))))
  (define mutant-samples+sample
    (hash-set mutant-samples mutant-program
              (set-add samples-for-mutant new-sample)))
  (struct-copy factory the-factory
               [mutant-samples mutant-samples+sample]))

;; factory?
;; mutant?
;; config?
;; (factory? -> (values config? factory?))
;; ->
;; factory?
;;
;; Spawns a mutant that attempts to follow a blame trail,
;; if the given `config` doesn't cause blame for `mutant-program`
;; then it calls `resample` to get a new configuration and try again.
(define (try-spawn-blame-following-mutant the-factory
                                          mutant-program
                                          config
                                          resample)
  (log-factory debug
               "    Trying to spawn blame-following mutant")
  (spawn-mutant the-factory
                (mutant-module mutant-program)
                (mutant-index mutant-program)
                config
                (make-blame-following-will/with-fallback
                 (λ (current-factory dead-proc)
                   ;; Try sampling another config
                   (define-values (new-sample new-factory)
                     (resample current-factory))
                   (try-spawn-blame-following-mutant new-factory
                                                     mutant-program
                                                     new-sample
                                                     resample)))))

;; factory? dead-mutant-process? -> factory?
;;
;; ASSUMPTIONS:
;; - the output of `dead-proc` has a blame label
;;
;; Spawns a mutant that follows the blame trail starting at `dead-proc`
(define (spawn-mutant/following-blame the-factory dead-proc blamed)
  (match-define (dead-mutant-process (mutant mod index) config _ id) dead-proc)
  (cond [(config-at-max-precision-for? blamed config)
         ;; Blamed region is at max ctcs, so the path ends here
         the-factory]
        [else
         (define config/blamed-region-ctc-strength-incremented
           (increment-config-precision-for blamed config))
         (define will:keep-following-blame
           (make-blame-following-will/with-fallback
            (λ (_ dead-successor)
              ;; The blame suddenly disappeared?
              (log-factory fatal
                           (failure-msg
                            "Blame disappeared while following blame trail.
Mutant: ~a @ ~a with id [~a] and config:
~a
Predecessor (id [~a]) blamed ~a and had config:
~a")
                           mod index (mutant-process-id dead-successor)
                           (mutant-process-config dead-successor)
                           id blamed
                           config)
              (exit 1))))
         (spawn-mutant the-factory
                       mod
                       index
                       config/blamed-region-ctc-strength-incremented
                       will:keep-following-blame)]))

;; (factory? dead-mutant-process? -> factory?)
;; ->
;; (factory? dead-mutant-process? -> factory?)
(define (make-blame-following-will/with-fallback no-blame-fallback)
  (λ (the-factory dead-proc)
    (cond
      [(try-get-blamed dead-proc)
       => (λ (blamed)
            ;; The mutant spawned hit on a blame trail, follow it
            (spawn-mutant/following-blame the-factory
                                          dead-proc
                                          blamed))]
      [else
       (no-blame-fallback the-factory dead-proc)])))



(define (spawn-mutant the-factory
                      module-to-mutate
                      mutation-index
                      precision-config
                      mutant-will)
  (let try-again ([current-factory the-factory])
    (define active-mutant-count (factory-active-mutant-count current-factory))
    (cond [(>= active-mutant-count (process-limit))
           (log-factory debug "    Mutants (~a) at process limit (~a)."
                        active-mutant-count
                        (process-limit))
           (sleep 2)
           (try-again (sweep-dead-mutants current-factory))]
          [else
           (match-define (factory (bench-info bench-name _)
                                  _
                                  active-mutants
                                  active-mutant-count
                                  _
                                  mutants-spawned)
             current-factory)
           (define outfile (build-path (data-output-dir)
                                       (format "~a_~a__index~a_~a.rktd"
                                               bench-name
                                               (basename module-to-mutate)
                                               mutation-index
                                               mutants-spawned)))
           (define mutant-ctl (spawn-mutant-runner bench-name
                                                   module-to-mutate
                                                   mutation-index
                                                   precision-config
                                                   outfile))
           (define mutant-proc
             (mutant-process (mutant module-to-mutate mutation-index)
                             precision-config
                             outfile
                             mutant-ctl
                             mutant-will
                             mutants-spawned))

           (log-factory info
                        "    Spawned mutant runner with id [~a] for ~a @ ~a > ~a."
                        mutants-spawned
                        module-to-mutate
                        mutation-index
                        outfile)

           (struct-copy factory current-factory
                        [active-mutants (set-add active-mutants mutant-proc)]
                        [active-mutant-count (add1 active-mutant-count)]
                        [total-mutants-spawned (add1 mutants-spawned)])])))



;; factory? -> factory?
(define (babysit-mutants the-factory)
  (let check-again ([current-factory the-factory])
    (define sweeped-factory (sweep-dead-mutants current-factory))
    (cond [(zero? (factory-active-mutant-count sweeped-factory))
           (log-factory info "Babysitting complete. All mutants are dead.")
           current-factory]
          [else
           (sleep 2)
           (check-again sweeped-factory)])))


;; factory? -> factory?
(define (sweep-dead-mutants the-factory)
  (log-factory debug "      Checking active mutant set for dead mutants...")
  (for/fold ([factory-so-far the-factory])
            ([mutant-proc (in-set (factory-active-mutants the-factory))])
    (define mutant-status ((mutant-process-ctl mutant-proc) 'status))
    (cond [(equal? mutant-status 'running)
           factory-so-far]
          [else
           (define mutant (mutant-process-mutant mutant-proc))
           (if (equal? mutant-status 'done-ok)
               (log-factory
                info
                "      Sweeping up dead mutant [~a]: ~a @ ~a, config ~a."
                (mutant-process-id mutant-proc)
                (mutant-module mutant)
                (mutant-index mutant)
                (mutant-process-config mutant-proc))
               (log-factory
                warning
                "*** WARNING: Runner errored on mutant ***
~a @ ~a with config ~a
**********\n\n"
                (mutant-module mutant)
                (mutant-index mutant)
                (mutant-process-config mutant-proc)))
           (process-dead-mutant factory-so-far mutant-proc)])))

;; factory? mutant-process? -> factory?
;; Note that processing a dead mutant may cause new mutants to be spawned,
;; since it executes the will of the dead mutant.
(define (process-dead-mutant the-factory mutant-proc)
  (match-define (mutant-process mutant config file _ will id) mutant-proc)
  (match-define (factory _ results active-mutants active-count _ _) the-factory)


  ;; Read the result of the mutant before possible consolidation
  (define result (get-mutant-result mutant-proc))
  (define dead-mutant-proc (dead-mutant-process mutant config result id))
  ;; Do the consolidation
  (define results+dead-mutant (add-mutant-result results dead-mutant-proc file))
  (define new-active-mutants (set-remove active-mutants mutant-proc))
  (define new-active-count (sub1 active-count))

  (define new-factory
    (struct-copy factory the-factory
                 [results results+dead-mutant]
                 [active-mutants new-active-mutants]
                 [active-mutant-count new-active-count]))

  (will new-factory dead-mutant-proc))

;; mutant-results? dead-mutant-process? path-string? -> mutant-results?
(define (add-mutant-result mutant-results dead-mutant-proc mutant-proc-file)
  (define mutant (dead-mutant-process-mutant dead-mutant-proc))
  (cond [(hash-has-key? mutant-results mutant)
         (define aggregate-results (hash-ref mutant-results mutant))
         (append-mutant-result! (dead-mutant-process-result dead-mutant-proc)
                                aggregate-results)
         (delete-file mutant-proc-file)
         mutant-results]
        [else
         ;; First process for this mutant to die, so its file
         ;; becomes the aggregate data file
         (hash-set mutant-results
                   mutant
                   (aggregate-mutant-result mutant mutant-proc-file))]))

;; path-string? natural? -> boolean?
(define (max-mutation-index-exceeded? module-to-mutate mutation-index
                                      [full-path? #f])
  (define path (if full-path?
                   module-to-mutate
                   (simplify-path (build-path benchmarks-dir-path
                                              module-to-mutate))))
  ;; `mutate-module` throws if index is too large, so just try
  ;; mutating to see whether or not it throws
  (with-handlers ([mutation-index-exception? (λ _ #t)])
    (make-mutated-module-runner path
                                path
                                '()
                                mutation-index
                                ;; A dummy config, it doesn't matter
                                (hash (path->string path) (hash)))
    #f))

;; result? aggregate-mutant-result? -> void
(define (append-mutant-result! result aggregate-results)
  (define aggregate-file (aggregate-mutant-result-file aggregate-results))
  (with-output-to-file aggregate-file
    #:exists 'append
    #:mode 'text
    (λ _ (writeln result))))

(define (get-mutant-result mutant-proc)
  (define path (mutant-process-file mutant-proc))
  (with-input-from-file path read))

;; dead-mutant-process? -> boolean?
(define (blame-outcome? dead-proc)
  (and (try-get-blamed dead-proc) #t))

;; dead-mutant-process? -> (vector (or/c symbol? path-string?))
(define/match (try-get-blamed dead-proc)
  [{(dead-mutant-process _ _ (list _ _ _ _ _ 'blamed blamed _) _)}
   blamed]
  [{(dead-mutant-process _ _ (? list?) _)} #f]
  [{(dead-mutant-process _ _ other _)}
   (log-factory fatal
                (failure-msg "Result read from mutant output was not a list.
Found: ~v
Dead mutant: ~v")
                other
                dead-proc)
   (exit 1)])

;; blame-info?: (vector/c symbol? path-string?)

;; config? blame-info? [any/c]
;; ->
;; (or/c config? any/c)
(define (config-ref/set config-hash blamed [set-value #f])
  (match-define (vector id mod) blamed)
  (define (missing-blame . _)
    (log-factory fatal
                 (failure-msg "Could not find blamed region in config.
Blamed: ~v
Config: ~v")
                 blamed
                 config-hash)
    (exit 1))
  (define mod-ids-hash (hash-ref config-hash mod missing-blame))
  (cond [set-value
         (define new-mod-ids-hash (hash-set mod-ids-hash id set-value))
         (hash-set config-hash mod new-mod-ids-hash)]
        [else
         (hash-ref mod-ids-hash id missing-blame)]))

;; blame-info? config? -> boolean?
(define (config-at-max-precision-for? blamed config)
  (equal? (config-ref/set config blamed) MAX-CONFIG))

;; blame-info? config? -> config?
(define (increment-config-precision-for blamed config)
  (define orig-precision (config-ref/set config blamed))
  (define orig-precision-index (index-of precision-configs orig-precision))
  (define new-precision (list-ref precision-configs
                                  (add1 orig-precision-index)))
  (config-ref/set config blamed new-precision))

(define (make-max-bench-config bench)
  (match-define (benchmark main others) bench)
  (define config/paths
    (for/hash ([mod (in-list (cons main others))])
      (define mod-path/simplified
        (simplify-path (build-path benchmarks-dir-path mod)))
      (define mod-ids
        (dynamic-require `(submod
                           ;; Must be file because `benchmarks-dir-path`
                           ;; is an absolute path
                           (file
                            ,(path->string mod-path/simplified))
                           contract-regions)
                         'regions))
      (values mod-path/simplified
              (for/hash ([id (in-list mod-ids)])
                (values id 'max)))))
  (make-config-safe-for-reading config/paths))

;; config? natural? -> (set config?)
;; Produces `n` random samples of the ctc precision config lattice
;; whose top element is `max-config`
(define (sample-config max-config n)
  (for/set ([i (in-range n)])
    (random-config-variant max-config)))

;; config? -> config?
(define (random-config-variant a-config)
  (for/hash ([(mod mod-config) (in-hash a-config)])
    (values mod
            (for/hash ([(id _) (in-hash mod-config)])
              (values id
                      (random-ref precision-configs))))))

(define (basename path)
  (define-values (_1 name _2) (split-path path))
  name)

(define (lookup-benchmark-with-name name)
  (hash-ref benchmarks name
            (λ _ (error 'run-all-mutants*configs
                        "Unknown benchmark: ~v" name))))


(module+ main
  (require racket/cmdline)
  (define bench-to-run (make-parameter #f))
  (command-line
   #:once-each
   [("-b" "--benchmark")
    name
    "Benchmark to run."
    (bench-to-run name)]
   [("-o" "--output-dir")
    dir
    "Data output directory."
    (data-output-dir dir)]
   [("-n" "--process-limit")
    n
    "Data output directory."
    (process-limit (string->number n))]
   [("-e" "--error-log")
    path
    "File to which to append mutant errors. Default: ./mutant-errors.txt"
    (mutant-error-log path)])
  (unless (bench-to-run)
    (error 'mutant-factory "Must provide benchmark to run."))
  (when (directory-exists? (data-output-dir))
    (eprintf "Output directory ~a already exists; remove? (y/n): "
             (data-output-dir))
    (match (read)
      [(or 'y 'yes) (delete-directory/files (data-output-dir))]
      [_ (eprintf "Not deleted.~n")]))
  (parameterize ([date-display-format 'iso-8601])
    (run-all-mutants*configs (bench-to-run))))
