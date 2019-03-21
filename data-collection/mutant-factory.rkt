#lang racket/base

(require "precision-lattice.rkt"
         "benchmarks.rkt"
         "../ctcs/current-precision-setting.rkt"
         "../mutate/mutation-runner.rkt"
         "../mutate/instrumented-module-runner.rkt"
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
         racket/list)

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
           increment-config-precision-for))


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
                             lattice-point
                             outfile)
  (call-with-output-file outfile #:mode 'text
    (λ (outfile-port)
      (call-with-output-file (mutant-error-log) #:mode 'text #:exists 'append
        (λ (error-log-port)
          (define config-hash (lattice-point->config-hash lattice-point))
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

;; will := (factory? dead-mutant-process? -> factory?)
;; result := list? (see `trace-collect.rkt`)

;; mutant: mutant?
;; config: lattice-point?
;; file:   path-string?
;; ctl:    process-ctl-fn? (see `process` docs)
;; will:   will?
;;     A transformation of the factory to be performed upon death of the mutant
(struct mutant-process (mutant config file ctl will) #:transparent)
;; result: result?
(struct dead-mutant-process (mutant config result) #:transparent)
(struct aggregate-mutant-result (mutant file) #:transparent)

;; (define mutant-results? (hash/c mutant? (set/c mutant-process?)))

;; name: string?
;; config-lattice: (set lattice-point?)
(struct bench-info (name config-lattice))

;; bench: bench-info?
;; results: mutant? |-> aggregate-mutant-result?
;;     The map from mutant to completed processes (which contain a data file).
;; active-mutants: (set mutant-process?)
;;     The set of actively running mutant processes.
;; active-mutant-count: natural?
;;     The size of `active-mutants`.
;;     INVARIANT: (= active-mutant-count (set-count active-mutants))
;; mutant-samples: mutant? |-> (set lattice-point?)
;;     The set of precision config samples checked for each mutant.
;; total-mutants-spawned: natural?
;;     Count of the total number of mutants spawned by this factory.
;;     This is primarily useful to making every new mutant file unique.
(struct factory (bench
                 results
                 active-mutants
                 active-mutant-count
                 mutant-samples
                 total-mutants-spawned))

;; string?
;; path-string?
;; ->
;; mutant-results?
(define (run-all-mutants*configs bench-name)
  (define bench (hash-ref benchmarks bench-name
                          (λ _ (error 'run-all-mutants*configs
                                      "Unknown benchmark: ~v" bench-name))))
  (define mutatable-modules (cons (benchmark-main bench)
                                  (benchmark-others bench)))
  (log-factory info "Running mutants of benchmark ~a, which has modules:~n~a"
                    bench-name
                    mutatable-modules)
  (log-factory debug "Creating config lattice...")
  (define config-lattice (precision-config-lattice mutatable-modules
                                                   precision-configs))
  (log-factory info "Built config lattice with ~a configurations."
                    (set-count (lattice-points config-lattice)))
  ;; Ensure data output directory exists
  (unless (directory-exists? (data-output-dir))
    (log-factory debug "Creating output directory ~a." (data-output-dir))
    (make-directory (data-output-dir)))

  (define factory-state
    (for/fold ([factory-state (factory (bench-info bench-name
                                                         config-lattice)
                                             (hash) (set) (set) (set))])
              ([module-to-mutate mutatable-modules])

      (log-factory info "Spawning mutant runners for module ~a."
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

    (log-factory info "Spawning runners for mutant ~a @ ~a."
                      module-to-mutate
                      mutation-index)

    (cond [(max-mutation-index-exceeded? module-to-mutate mutation-index)
           the-factory]

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
(define (maybe-spawn-configured-mutants the-factory mutant-program)
  (match-define (mutant module-to-mutate mutation-index) mutant-program)
  (log-factory debug
               "Spawning test mutant for ~a @ ~a."
               module-to-mutate
               mutation-index)
  (define max-config
    (lattice-max (bench-info-config-lattice (factory-bench the-factory))))
  (spawn-mutant the-factory
                module-to-mutate
                mutation-index
                max-config
                ;; mutant will
                (λ (the-factory dead-proc)
                  (if (blame-outcome? dead-proc)
                      (spawn-mutants/precision-sampling the-factory
                                                        mutant-program)
                      the-factory))))

;; path-string? mutant? -> factory?
(define (spawn-mutants/precision-sampling the-factory mutant-program)
  (define lattice (bench-info-config-lattice (factory-bench the-factory)))
  (define samples (sample-lattice lattice N-SAMPLES))
  (define (resample a-factory)
    (define sample (first (sample-lattice lattice 1)))
    (define samples-seen (hash-ref (factory-mutant-samples a-factory)
                                   mutant-program))
    (cond [(set-member? samples-seen sample)
           (resample a-factory)]
          [else
           (values sample
                   (add-mutant-sample a-factory mutant-program sample))]))
  (for/fold ([current-factory the-factory])
            ([sampled-config (in-list samples)])
    (define factory+sample
      (add-mutant-sample current-factory mutant-program sampled-config))
    (try-spawn-blame-following-mutant factory+sample
                                      mutant-program
                                      sampled-config
                                      resample)))

(define (add-mutant-sample the-factory mutant-program new-sample)
  (define mutant-samples (factory-mutant-samples the-factory))
  (define samples-for-mutant (hash-ref mutant-samples mutant-program))
  (define mutant-samples+sample
    (hash-set mutant-samples mutant-program
              (set-add samples-for-mutant new-sample)))
  (struct-copy factory the-factory
               [mutant-samples mutant-samples+sample]))

;; factory?
;; mutant?
;; lattice-point?
;; (factory? -> (values lattice-point? factory?))
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
  (spawn-mutant the-factory
                (mutant-module mutant-program)
                (mutant-index mutant-program)
                config
                (make-blame-following-will/with-fallback
                 (λ (the-factory* dead-proc)
                   ;; Try sampling another config
                   (define-values (new-sample new-factory)
                     (resample the-factory*))
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
  (match-define (mutant-process (mutant mod index) config _ _ _) dead-proc)
  (cond [(config-at-max-precision-for? blamed config)
         ;; Blamed region is at max ctcs, so the path ends here
         the-factory]
        [else
         (define config/blamed-region-ctc-strength-incremented
           (increment-config-precision-for blamed config))
         (define will:keep-following-blame
           (make-blame-following-will/with-fallback
            (λ (the-factory* dead-successor)
              ;; The blame suddenly disappeared?
              (log-factory fatal
                           (failure-msg
                            "Blame disappeared while following blame trail.
Mutant: ~a @ ~a with config:
~a
Predecessor blamed ~a and had config:
~a")
                           mod index
                           (lattice-point-value
                            (mutant-process-config dead-successor))
                           blamed
                           (lattice-point-value config))
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
  (let try-again ([the-factory the-factory])
    (define active-mutant-count (factory-active-mutant-count the-factory))
    (cond [(>= active-mutant-count (process-limit))
           (log-factory debug "Mutants (~a) at process limit (~a)."
                        active-mutant-count
                        (process-limit))
           (sleep 2)
           (try-again (sweep-dead-mutants the-factory))]
          [else
           (match-define (factory (bench-info bench-name _)
                                  _
                                  active-mutants
                                  active-mutant-count
                                  _
                                  mutants-spawned)
             the-factory)
           (define outfile (build-path (data-output-dir)
                                       (format "~a_~a__index~a_~a.rktd"
                                               bench-name
                                               (basename module-to-mutate)
                                               mutation-index
                                               mutants-spawned)))
           (log-factory info
                        "Spawning mutant runner #~a for ~a @ ~a > ~a."
                        mutants-spawned
                        module-to-mutate
                        mutation-index
                        outfile)
           (define mutant-ctl (spawn-mutant-runner bench-name
                                                   module-to-mutate
                                                   mutation-index
                                                   precision-config
                                                   outfile))
           (define mutant
             (mutant-process (mutant module-to-mutate mutation-index)
                             precision-config
                             outfile
                             mutant-ctl
                             mutant-will))
           (struct-copy factory the-factory
                        [active-mutants (set-add active-mutants mutant)]
                        [active-mutant-count (add1 active-mutant-count)]
                        [total-mutants-spawned (add1 mutants-spawned)])])))



;; factory? -> factory?
(define (babysit-mutants the-factory)
  (let check-again ([current-factory the-factory])
    (define sweeped-factory (sweep-dead-mutants current-factory))
    (cond [(zero? (factory-active-mutant-count sweeped-factory))
           (log-factory info "Babysitting complete. All mutants are dead.")
           the-factory]
          [else
           (sleep 2)
           (check-again sweeped-factory)])))


;; factory? -> factory?
(define (sweep-dead-mutants the-factory)
  (log-factory debug "Checking active mutant set for dead mutants...")
  (for/fold ([factory-so-far the-factory])
            ([mutant-proc (in-set (factory-active-mutants the-factory))])
    (define mutant-status ((mutant-process-ctl mutant-proc) 'status))
    (cond [(equal? mutant-status 'running)
           factory-so-far]
          [else
           (define mutant (mutant-process-mutant mutant-proc))
           (if (equal? mutant-status 'done-ok)
               (log-factory info
                            (format
                             "Sweeping up dead mutant: ~a @ ~a with config ~a."
                             (mutant-module mutant)
                             (mutant-index mutant)
                             ;; Don't print the paths, they're huge
                             (lattice-point-value
                              (mutant-process-config mutant-proc))))
               (log-factory warning
                            "*** WARNING: Runner errored on mutant ***
~a @ ~a with config ~a
**********\n\n"
                            (mutant-module mutant)
                            (mutant-index mutant)
                            ;; Don't print the paths, they're huge
                            (lattice-point-value
                             (mutant-process-config mutant-proc))))
           (process-dead-mutant factory-so-far mutant-proc)])))

;; factory? mutant-process? -> factory?
;; Note that processing a dead mutant may cause new mutants to be spawned,
;; since it executes the will of the dead mutant.
(define (process-dead-mutant the-factory mutant-proc)
  (match-define (mutant-process mutant config file _ will) mutant-proc)
  ;; Read the result of the mutant before possible consolidation
  (define result (get-mutant-result mutant-proc))
  (define dead-mutant-proc (dead-mutant-process mutant config result))
  ;; Do the consolidation
  (define factory+result (add-mutant-result the-factory dead-mutant-proc file))

  (will factory+result dead-mutant-proc))

;; factory? dead-mutant-process? path-string? -> factory?
(define (add-mutant-result the-factory dead-mutant-proc mutant-proc-file)
  (define mutant (dead-mutant-process-mutant dead-mutant-proc))
  (define mutant-results (factory-results the-factory))
  (define mutant-results+dead-mutant-proc
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
  (struct-copy factory the-factory
               [results mutant-results+dead-mutant-proc]))

;; path-string? natural? -> boolean?
(define (max-mutation-index-exceeded? module-to-mutate mutation-index
                                      [full-path? #f])
  (define path (if full-path?
                   module-to-mutate
                   (build-path benchmarks-dir-path
                               module-to-mutate)))
  ;; `mutate-module` throws if index is too large, so just try
  ;; mutating to see whether or not it throws
  (with-handlers ([mutation-index-exception? (λ _ #t)])
    (make-mutated-module-runner path
                                path
                                '()
                                mutation-index
                                (hash))
    #f))

;; lattice-point? -> (hash/c path-string? symbol?)
(define/match (lattice-point->config-hash point)
  [{(lattice-point bench-config _)}
   (for/hash ([config (in-set bench-config)])
      (match-define (mod-config mod level) config)
      (values mod level))])

;; result? aggregate-mutant-result? -> void
(define (append-mutant-result! result aggregate-results)
  (define aggregate-file (aggregate-mutant-result-file aggregate-results))
  (with-output-to-file aggregate-file
    #:exists 'append
    #:mode 'text
    (writeln result)))

(define (get-mutant-result mutant-proc)
  (define path (mutant-process-file mutant-proc))
  (with-input-from-file path read))

;; dead-mutant-process? -> boolean?
(define (blame-outcome? dead-proc)
  (and (try-get-blamed dead-proc) #t))

;; dead-mutant-process? -> (or/c symbol? path-string?)
(define/match (try-get-blamed dead-proc)
  [{(dead-mutant-process _ _ (list _ _ _ _ _ 'blamed blamed _))} blamed]
  [{(dead-mutant-process _ _ (? list?))} #f]
  [{(dead-mutant-process _ _ other)}
   (log-factory fatal
                (failure-msg "Result read from mutant output was not a list.
Found: ~v
Dead mutant: ~v")
                other
                dead-proc)
   (exit 1)])

(define (config-ref/set config blamed [set-value #f])
  (match-define (vector id mod) blamed)
  (define config-hash (lattice-point-value config))
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

(define (config-at-max-precision-for? blamed config)
  (equal? (config-ref/set config blamed) MAX-CONFIG))

(define (increment-config-precision-for blamed config)
  (define orig-precision (config-ref/set config blamed))
  (define orig-precision-index (index-of precision-configs orig-precision))
  (define new-precision (list-ref precision-configs
                                  (add1 orig-precision-index)))
  (config-ref/set config blamed new-precision))


(define (basename path)
  (define-values (_1 name _2) (split-path path))
  name)


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
