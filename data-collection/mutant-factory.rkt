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
         racket/port)

(define mutant-runner-path (make-parameter "mutant-runner.rkt"))
(define process-limit (make-parameter 10))
(define temp-output-dir (make-parameter "~/mutant-data/"))

(define (spawn-mutant-runner benchmark-name
                             module-to-mutate
                             mutation-index
                             lattice-point
                             outfile)
  (define config-hash (lattice-point->config-hash lattice-point))
  (match-define (list runner-out runner-in _ runner-err runner-ctl)
    (process (format "racket '~a' -b '~a' -m '~a' -i ~a -o '~a'"
                     (mutant-runner-path)
                     benchmark-name
                     module-to-mutate
                     mutation-index
                     outfile)))
  (write (serialize config-hash) runner-in)
  (close-input-port runner-out)
  (close-input-port runner-in)
  (close-input-port runner-err)
  runner-ctl)

(struct mutant (module index))
(struct mutant-process (mutant config file ctl))

(define (run-all-mutants*configs bench-name)
  (define bench (hash-ref benchmarks bench-name
                          (λ _ (error 'run-all-mutants*configs
                                      "Unknown benchmark: ~v" bench-name))))
  (define mutatable-modules (cons (benchmark-main bench)
                                  (benchmark-others bench)))
  ;; lltodo: this might take a while, maybe report progress?
  (define config-lattice (precision-config-lattice mutatable-modules
                                                   precision-configs))
  ;; Ensure data output directory exists
  (unless (directory-exists? (temp-output-dir))
    (make-directory (temp-output-dir)))

  (define-values (mutant-results active-mutants)
    (for/fold (;; mutant-results: mutant? -> (set mutant-process?)
               [mutant-results (hash)]
               ;; active-mutants: (set mutant-process?)
               [active-mutants (set)])
              ([module-to-mutate mutatable-modules])

      (spawn-mutants/of-module module-to-mutate
                               bench-name
                               config-lattice
                               mutant-results
                               active-mutants)))

  (babysit-mutants mutant-results active-mutants))

;; path-string?
;; string?
;; (set/c lattice-point?)
;; (hash/c mutant? (set/c mutant-process?))
;; (set/c mutant-process?)
;; ->
;; (hash/c mutant? (set/c mutant-process?))
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

    ;; Every mutant we try to consolidate mutant result files, to keep
    ;; the number of files generated low(ish)
    (define mutant-results/consolidated
      (try-consolidate-mutant-results mutant-results))

    (cond [(max-mutation-index-exceeded? module-to-mutate mutation-index)
           (values mutant-results/consolidated active-mutants)]

          [else
           (define-values (mutant-results active-mutants)
             (spawn-mutant/per-precision-in config-lattice
                                            module-to-mutate
                                            mutation-index
                                            bench-name
                                            mutant-results/consolidated
                                            active-mutants))

           (next-mutant (add1 mutation-index)
                        mutant-results
                        active-mutants)])))

;; (set/c lattice-point?)
;; path-string?
;; natural?
;; string?
;; (hash/c mutant? (set/c mutant-process?))
;; (set/c mutant-process?)
;; ->
;; (hash/c mutant? (set/c mutant-process?))
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

    (define-values (mutant-results*
                    active-mutants*
                    active-mutant-count*)
      (maybe-sweep-dead-mutants mutant-results
                                active-mutants
                                active-mutant-count))
    (define outfile (build-path (temp-output-dir)
                                (format "~a__index~a_~a"
                                        (regexp-replace* "/"
                                                         module-to-mutate
                                                         "_")
                                        mutation-index
                                        i)))
    (spawn-mutant-runner bench-name
                         module-to-mutate
                         mutation-index
                         precision-config
                         outfile)))



;; (hash/c mutant? (set/c mutant-process?))
;; (set/c mutant-process?)
;; ->
;; (hash/c mutant? (set/c mutant-process?))
(define (babysit-mutants mutant-results active-mutants)
  (let loop ([active active-mutants]
             [results mutant-results])
    (sleep 1)
    (define-values (mutant-results*
                    active-mutants*
                    active-mutant-count*)
      (maybe-sweep-dead-mutants results active
                                #:force? #t))
    (if (zero? active-mutant-count*)
        mutant-results*
        (loop active-mutants*
              mutant-results*))))

(define (maybe-sweep-dead-mutants mutant-results
                                  active-mutants
                                  [active-mutant-count #f]
                                  #:force? [force? #f])
  (define active-mutant-count* (or active-mutant-count
                                   (set-count active-mutants)))
  (if (or force?
          (>= active-mutant-count* (process-limit)))
      (sweep-dead-mutants mutant-results active-mutants active-mutant-count*)
      (values mutant-results active-mutants active-mutant-count*)))

(define (sweep-dead-mutants mutant-results active-mutants active-mutant-count)
  (for/fold ([new-results mutant-results]
             [new-active-mutant-count 0]
             [new-active-mutants (set)])
            ([mutant-proc active-mutants])
    (define mutant-status ((mutant-process-ctl mutant-proc) 'status))
    (if (equal? mutant-status 'running)
        (values new-results
                (add1 new-active-mutant-count)
                (set-add new-active-mutants mutant-proc))
        (values (add-mutant-result new-results mutant-proc)
                new-active-mutant-count
                new-active-mutants))))

(define (add-mutant-result mutant-results mutant-proc)
  (define mutant (mutant-process-mutant mutant-proc))
  (define results (hash-ref mutant-results mutant
                            (λ _ (set))))
  (hash-set mutant-results
            mutant
            (set-add results mutant-proc)))

(define (max-mutation-index-exceeded? module-to-mutate mutation-index)
  ;; `mutate-module` throws if index is too large, so just try
  ;; mutating to see whether or not it throws
  (with-handlers ([mutation-index-exception? (λ _ #t)])
    (make-instrumented-module-runner module-to-mutate
                                     '()
                                     (match-lambda**
                                      [{(== module-to-mutate) stx}
                                       (mutate-module stx mutation-index)
                                       stx]
                                      [{_ stx} stx]))
    #f))

(define/match (lattice-point->config-hash point)
  [{(lattice-point bench-config _)}
   (for/hash ([config (in-set bench-config)])
      (match-define (mod-config mod level) config)
      (values mod level))])

;; (hash/c mutant? (set/c mutant-process?))
;; ->
;; (hash/c mutant? (set/c mutant-process?))
(define (try-consolidate-mutant-results mutant-results)
  (for/hash ([(mutant processes) (in-hash mutant-results)])
    (if (> (set-count processes) 1)
        (values mutant (set (consolidate-mutant-results processes)))
        (values mutant processes))))

;; (and/c (set/c mutant-process?) (count>/c 1))
;; ->
;; (and/c (set/c mutant-process?) (count=/c 1))
(define (consolidate-mutant-results mutant-processes)
  ;; Just pick an output file to be the consolidated one
  (match-define (mutant-process _ _ consolidated-file _)
    (set-first mutant-processes))
  (define out (open-output-file consolidated-file #:exists 'append))
  (for ([proc (in-set (set-rest mutant-processes))])
    (define other-file (mutant-process-file proc))
    (define in (open-input-file other-file))
    (copy-port in out)
    (newline out)
    (close-input-port in)
    (delete-file other-file))
  (close-output-port out))
