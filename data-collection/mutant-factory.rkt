#lang racket/base

(require "precision-lattice.rkt"
         "benchmarks.rkt"
         "../ctcs/current-precision-setting.rkt"
         "../mutate/mutation-runner.rkt"
         "../mutate/instrumented-module-runner.rkt"
         racket/match
         racket/system
         racket/serialize
         racket/set
         racket/port
         racket/file)

(module+ test
  (provide max-mutation-index-exceeded?
           (struct-out mutant)
           (struct-out mutant-process)
           try-consolidate-mutant-results
           process-limit
           maybe-sweep-dead-mutants
           add-mutant-result))

(define mutant-runner-path (make-parameter "mutant-runner.rkt"))
(define process-limit (make-parameter 3))
(define data-output-dir (make-parameter "./mutant-data"))
(define path-to-benchmarks (make-parameter "../benchmarks"))

(define-logger factory)

(define (spawn-mutant-runner benchmark-name
                             module-to-mutate
                             mutation-index
                             lattice-point
                             outfile)
  (define config-hash (lattice-point->config-hash lattice-point))
  (match-define (list runner-out runner-in _ runner-err runner-ctl)
    (process (format "racket-7.2 '~a' -b '~a' -m '~a' -i ~a > '~a'"
                     (mutant-runner-path)
                     benchmark-name
                     module-to-mutate
                     mutation-index
                     outfile)))
  (write (serialize config-hash) runner-in)
  (close-input-port runner-out)
  (close-output-port runner-in)
  (close-input-port runner-err)
  runner-ctl)

(struct mutant (module index) #:transparent)
(struct mutant-process (mutant config file ctl) #:transparent)
;; (define mutant-results? (hash/c mutant? (set/c mutant-process?)))

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
  (log-factory-info "Running mutants of benchmark ~a, which has modules:~n~a"
                    bench-name
                    mutatable-modules)
  (log-factory-debug "Creating config lattice...")
  (define config-lattice (precision-config-lattice mutatable-modules
                                                   precision-configs))
  (log-factory-info "Built config lattice with ~a configurations."
                    (set-count config-lattice))
  ;; Ensure data output directory exists
  (unless (directory-exists? (data-output-dir))
    (log-factory-debug "Creating output directory ~a."
                       (data-output-dir))
    (make-directory (data-output-dir)))

  (define-values (mutant-results active-mutants)
    (for/fold (;; mutant-results: mutant? -> (set mutant-process?)
               [mutant-results (hash)]
               ;; active-mutants: (set mutant-process?)
               [active-mutants (set)])
              ([module-to-mutate mutatable-modules])

      (log-factory-info "Spawning mutant runners for module ~a."
                        module-to-mutate)
      (spawn-mutants/of-module module-to-mutate
                               bench-name
                               config-lattice
                               mutant-results
                               active-mutants)))

  (log-factory-info "Finished spawning all mutant runners.")
  (log-factory-debug "Active mutant count: ~a." (set-count active-mutants))
  (babysit-mutants mutant-results active-mutants))

;; path-string?
;; string?
;; (set/c lattice-point?)
;; mutant-results?
;; (set/c mutant-process?)
;; ->
;; mutant-results?
;; (set/c mutant-process?)
(define (spawn-mutants/of-module module-to-mutate
                                 bench-name
                                 config-lattice
                                 mutant-results
                                 active-mutants)
  ;; This must be an unbounded loop, number of mutants unknown
  (let next-mutant ([mutation-index 0]
                    [mutant-results mutant-results]
                    [active-mutants active-mutants])

    (log-factory-info "Spawning runners for mutant ~a @ ~a."
                      module-to-mutate
                      mutation-index)

    (cond [(max-mutation-index-exceeded? module-to-mutate mutation-index)
           (values mutant-results active-mutants)]

          [else
           (define-values (mutant-results* active-mutants*)
             (spawn-mutant/per-precision-in config-lattice
                                            module-to-mutate
                                            mutation-index
                                            bench-name
                                            mutant-results
                                            active-mutants))

           (next-mutant (add1 mutation-index)
                        mutant-results*
                        active-mutants*)])))

;; (set/c lattice-point?)
;; path-string?
;; natural?
;; string?
;; mutant-results?
;; (set/c mutant-process?)
;; ->
;; mutant-results?
;; (set/c mutant-process?)
(define (spawn-mutant/per-precision-in config-lattice
                                       module-to-mutate
                                       mutation-index
                                       bench-name
                                       mutant-results
                                       active-mutants)
  (for/fold ([mutant-results mutant-results]
             [active-mutants active-mutants]
             [active-mutant-count (set-count active-mutants)]
             #:result (values mutant-results active-mutants))
            ([precision-config config-lattice]
             [i (in-naturals)])

    (define-values (mutant-results* active-mutants* active-mutant-count*)
      (maybe-sweep-dead-mutants mutant-results
                                active-mutants
                                active-mutant-count))
    (log-factory-debug "Active mutants: ~a." active-mutant-count*)
    (define outfile (build-path (data-output-dir)
                                (format "~a_~a__index~a_~a.rktd"
                                        bench-name
                                        (basename module-to-mutate)
                                        mutation-index
                                        i)))
    (log-factory-info "Spawning configured mutant runner #~a for ~a @ ~a > ~a."
                       i
                       module-to-mutate
                       mutation-index
                       outfile)
    (define mutant-ctl (spawn-mutant-runner bench-name
                                            module-to-mutate
                                            mutation-index
                                            precision-config
                                            outfile))
    (values mutant-results*
            (set-add active-mutants*
                     (mutant-process (mutant module-to-mutate mutation-index)
                                     precision-config
                                     outfile
                                     mutant-ctl))
            (add1 active-mutant-count*))))



;; mutant-results?
;; (set/c mutant-process?)
;; ->
;; mutant-results?
(define (babysit-mutants mutant-results active-mutants)
  (let loop ([active active-mutants]
             [results mutant-results])
    (sleep 1)
    (define-values (mutant-results* active-mutants* active-mutant-count*)
      (maybe-sweep-dead-mutants results active #:force? #t))
    (cond [(zero? active-mutant-count*)
           (log-factory-info "Performing final consolidation of results...")
           (for/fold ([mutant-results** mutant-results*])
                     ([mutant (in-hash-keys mutant-results*)])
             (try-consolidate-mutant-results mutant-results** mutant))]
          [else
           (loop active-mutants*
                 mutant-results*)])))

;; mutant-results?
;; (set/c mutant-process?)
;; [natural?]
;; [#:force? boolean?]
;; ->
;; mutant-results?
;; (set/c mutant-process?)
;; natural?
(define (maybe-sweep-dead-mutants mutant-results
                                  active-mutants
                                  [active-mutant-count #f]
                                  #:force? [force? #f]
                                  #:block? [block? #t])
  (define active-mutant-count* (or active-mutant-count
                                   (set-count active-mutants)))
  (cond [(or force?
             (>= active-mutant-count* (process-limit)))
         (log-factory-debug "Mutants (~a) at process limit (~a)."
                            active-mutant-count*
                            (process-limit))
         (sleep 2)
         (define-values (r a c)
           (sweep-dead-mutants mutant-results
                               active-mutants
                               active-mutant-count*))
         (if block?
             (maybe-sweep-dead-mutants r a c)
             (values r a c))]
        [else
         (values mutant-results active-mutants active-mutant-count*)]))

;; mutant-results?
;; (set/c mutant-process?)
;; natural?
;; ->
;; mutant-results?
;; (set/c mutant-process?)
;; natural?
(define (sweep-dead-mutants mutant-results active-mutants active-mutant-count)
  (log-factory-debug "Checking active mutant set for dead mutants...")
  (for/fold ([new-results mutant-results]
             [new-active-mutants (set)]
             [new-active-mutant-count 0])
            ([mutant-proc active-mutants])
    (define mutant-status ((mutant-process-ctl mutant-proc) 'status))
    (cond [(equal? mutant-status 'running)
           (values new-results
                   (set-add new-active-mutants mutant-proc)
                   (add1 new-active-mutant-count))]
          [else
           (define mutant (mutant-process-mutant mutant-proc))
           (log-factory-info
            (format
             "Sweeping up dead mutant: ~a @ ~a with config ~a"
             (mutant-module mutant)
             (mutant-index mutant)
             ;; Don't print the paths, they're huge
             (lattice-point-value (mutant-process-config mutant-proc))))
           (define mutant-results* (add-mutant-result new-results mutant-proc))
           ;; Try to merge output files with other dead processes for
           ;; same mutant, to keep the number of files low
           (values (try-consolidate-mutant-results mutant-results* mutant)
                   new-active-mutants
                   new-active-mutant-count)])))

;; mutant-results? mutant-process? -> mutant-results?
(define (add-mutant-result mutant-results mutant-proc)
  (define mutant (mutant-process-mutant mutant-proc))
  (define results (hash-ref mutant-results mutant
                            (λ _ (set))))
  (hash-set mutant-results
            mutant
            (set-add results mutant-proc)))

;; path-string? natural? -> boolean?
(define (max-mutation-index-exceeded? module-to-mutate mutation-index
                                      [full-path? #f])
  (define path (if full-path?
                   module-to-mutate
                   (build-path (path-to-benchmarks)
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

;; mutant-results?
;; mutant?
;; ->
;; mutant-results?
;;
;; ASSUMPTIONS:
;; - All of the mutant-processes in mutant-results have completed,
;;   ie their `ctl` does not return 'running
;; - `mutant-results` contains `mutant`
(define (try-consolidate-mutant-results mutant-results mutant)
  (log-factory-debug "Trying to consolidate mutant output files...")
  (define processes (hash-ref mutant-results mutant))
  (cond [(> (set-count processes) 1)
         (log-factory-debug
          "Consolidating results of mutant: ~a @ ~a, which has ~a processes."
          (mutant-module mutant)
          (mutant-index mutant)
          (set-count processes))
         (hash-set mutant-results
                   mutant
                   (set (consolidate-mutant-results processes)))]
        [else
         mutant-results]))

;; (and/c (set/c mutant-process?) (count>/c 1))
;; ->
;; mutant-process?
(define (consolidate-mutant-results mutant-processes)
  ;; Just pick an output file to be the consolidated one
  (define consolidated-mutant-process (set-first mutant-processes))
  (match-define (mutant-process _ _ consolidated-file _)
    consolidated-mutant-process)
  (log-factory-debug
   "Picked mutant output file ~a to preserve..."
   consolidated-file)
  (define out (open-output-file consolidated-file #:exists 'append))
  (for ([proc (in-set (set-rest mutant-processes))])
    (define other-file (mutant-process-file proc))
    (log-factory-debug "Consolidating file ~a." other-file)
    (define in (open-input-file other-file))
    (copy-port in out)
    (close-input-port in)
    (delete-file other-file))
  (close-output-port out)
  consolidated-mutant-process)


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
   [("-d" "--benchmarks-directory")
    dir
    "Directory containing benchmarks. Default: ../benchmarks"
    (path-to-benchmarks dir)])
  (unless (bench-to-run)
    (error 'mutant-factory "Must provide benchmark to run."))
  (when (directory-exists? (data-output-dir))
    (eprintf "Output directory ~a already exists; remove? (y/n): "
             (data-output-dir))
    (match (read)
      [(or 'y 'yes) (delete-directory/files (data-output-dir))]
      [_ (eprintf "Not deleted.~n")]))
  (run-all-mutants*configs (bench-to-run)))
