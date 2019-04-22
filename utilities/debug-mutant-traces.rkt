#lang racket

(require "debug-traces.rkt"
         racket/runtime-path
         (for-syntax racket/base)
         racket/logging
         "debug-mutant-call-graph.rkt")

(define-runtime-module-path-index debug-mutant-mpi "debug-mutant.rkt")

;; -------------------- Other weaker configs --------------------
;; original: diverges at 1703 / 1750
#;"#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . types) (explore . none) (atom-eval . none) (next . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . #hash((make-let . none) (make-lambda . none) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . max) (standard-example . types) (make-ref . max) (make-call . max) (new-label . none) (main . types))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . max) (benv-extend . max) (benv-lookup . none) (benv-extend* . types) (empty-benv . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . #hash((store-update* . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . none) (store-join . max) (d-join . max) (store-update . none) (d-bot . none) (store-lookup . types) (empty-store . types))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . #hash((time-zero . types) (take* . max) (tick . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . types) (alloc . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . #hash((monovariant-value . types) (summarize . max) (format-mono-store . types) (analyze . types) (empty-mono-store . none) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . max) (monovariant-store . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . types))))"
;; this diverges at 1600 / 1750
#;"#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . max) (explore . max) (atom-eval . none) (next . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . #hash((make-let . max) (make-lambda . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . max) (standard-example . max) (make-ref . max) (make-call . max) (new-label . max) (main . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . max) (benv-extend . max) (benv-lookup . max) (benv-extend* . max) (empty-benv . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . #hash((store-update* . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . max) (store-join . max) (d-join . max) (store-update . max) (d-bot . max) (store-lookup . max) (empty-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . #hash((time-zero . max) (take* . max) (tick . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . max) (alloc . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . #hash((monovariant-value . max) (summarize . max) (format-mono-store . max) (analyze . max) (empty-mono-store . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . max) (monovariant-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . max))))"
;; Diverges at 1726 / 1750
#;"#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . max) (explore . max) (atom-eval . none) (next . types))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . #hash((make-let . max) (make-lambda . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . max) (standard-example . max) (make-ref . max) (make-call . max) (new-label . max) (main . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . max) (benv-extend . max) (benv-lookup . max) (benv-extend* . max) (empty-benv . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . #hash((store-update* . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . max) (store-join . max) (d-join . max) (store-update . max) (d-bot . max) (store-lookup . max) (empty-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . #hash((time-zero . max) (take* . max) (tick . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . max) (alloc . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . #hash((monovariant-value . max) (summarize . max) (format-mono-store . max) (analyze . max) (empty-mono-store . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . max) (monovariant-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . max))))"
;; Doesn't diverge
#;"#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . max) (explore . max) (atom-eval . max) (next . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . #hash((make-let . max) (make-lambda . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . max) (standard-example . max) (make-ref . max) (make-call . max) (new-label . max) (main . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . max) (benv-extend . max) (benv-lookup . max) (benv-extend* . max) (empty-benv . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . #hash((store-update* . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . max) (store-join . max) (d-join . max) (store-update . max) (d-bot . max) (store-lookup . max) (empty-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . #hash((time-zero . max) (take* . max) (tick . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . max) (alloc . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . #hash((monovariant-value . max) (summarize . max) (format-mono-store . max) (analyze . max) (empty-mono-store . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . max) (monovariant-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . max))))"
;; this aligns with max (ie doesn't diverge)
#;"#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . max) (explore . max) (atom-eval . types) (next . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . #hash((make-let . max) (make-lambda . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . max) (standard-example . max) (make-ref . max) (make-call . max) (new-label . max) (main . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . max) (benv-extend . max) (benv-lookup . max) (benv-extend* . max) (empty-benv . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . #hash((store-update* . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . max) (store-join . max) (d-join . max) (store-update . max) (d-bot . max) (store-lookup . max) (empty-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . #hash((time-zero . max) (take* . max) (tick . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . max) (alloc . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . #hash((monovariant-value . max) (summarize . max) (format-mono-store . max) (analyze . max) (empty-mono-store . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . max) (monovariant-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . max))))"

;; -------------------- Other stronger configs --------------------
;; original max
#;"#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ai.rkt> . max) (explore . max) (atom-eval . max) (next . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . #hash((make-let . max) (make-lambda . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . max) (standard-example . max) (make-ref . max) (make-call . max) (new-label . max) (main . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/benv.rkt> . max) (benv-extend . max) (benv-lookup . max) (benv-extend* . max) (empty-benv . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . #hash((store-update* . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/denotable.rkt> . max) (store-join . max) (d-join . max) (store-update . max) (d-bot . max) (store-lookup . max) (empty-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . #hash((time-zero . max) (take* . max) (tick . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/time.rkt> . max) (alloc . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . #hash((monovariant-value . max) (summarize . max) (format-mono-store . max) (analyze . max) (empty-mono-store . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/ui.rkt> . max) (monovariant-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/structs.rkt> . max))))"

(define (weaker-call-graph)
  (debug-call-graph "kcfa-debug-diverge"
                    "#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt> . #hash((make-let . max) (atom-eval . none) (make-lambda . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt> . max) (standard-example . max) (make-ref . max) (make-call . max) (new-label . max) (main . max))))"))

(define (run-debug-modules #:dump-weaker [weaker-path #f]
                           #:dump-stronger [stronger-path #f]
                           #:trailing-lines [trailing-lines 5]
                           #:print-traces? [print-traces? #t]
                           #:show-prefix? [show-prefix? #f]
                           #:diff-effective-Δ? [diff-effective-Δ? #f]
                           #:print-Δs? [print-Δs? #f])
  (define weaker-config
    "#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . #hash((make-let . none) (atom-eval . none) (make-lambda . none) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . none) (standard-example . none) (make-ref . none) (make-call . none) (new-label . none) (main . none))))")
  (define max-config
    ;; also diverges:
    "#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . #hash((make-let . none) (atom-eval . max) (make-lambda . none) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa-debug-diverge/untyped/main.rkt> . none) (standard-example . none) (make-ref . none) (make-call . none) (new-label . none) (main . none))))")
  (compare-traces "kcfa"
                  "kcfa/untyped/main.rkt"
                  3
                  ;; weaker-config
                  ;; max-config
                  "#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ai.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ai.rkt> . types) (explore . none) (atom-eval . none) (next . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt> . #hash((make-let . none) (make-lambda . none) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt> . max) (standard-example . types) (make-ref . max) (make-call . max) (new-label . none) (main . types))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/benv.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/benv.rkt> . max) (benv-extend . max) (benv-lookup . none) (benv-extend* . types) (empty-benv . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/denotable.rkt> . #hash((store-update* . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/denotable.rkt> . none) (store-join . max) (d-join . max) (store-update . none) (d-bot . none) (store-lookup . types) (empty-store . types))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/time.rkt> . #hash((time-zero . types) (take* . max) (tick . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/time.rkt> . types) (alloc . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ui.rkt> . #hash((monovariant-value . types) (summarize . max) (format-mono-store . types) (analyze . types) (empty-mono-store . none) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ui.rkt> . max) (monovariant-store . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/structs.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/structs.rkt> . types))))"
                  "#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ai.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ai.rkt> . max) (explore . max) (atom-eval . max) (next . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt> . #hash((make-let . max) (make-lambda . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt> . max) (standard-example . max) (make-ref . max) (make-call . max) (new-label . max) (main . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/benv.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/benv.rkt> . max) (benv-extend . max) (benv-lookup . max) (benv-extend* . max) (empty-benv . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/denotable.rkt> . #hash((store-update* . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/denotable.rkt> . max) (store-join . max) (d-join . max) (store-update . max) (d-bot . max) (store-lookup . max) (empty-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/time.rkt> . #hash((time-zero . max) (take* . max) (tick . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/time.rkt> . max) (alloc . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ui.rkt> . #hash((monovariant-value . max) (summarize . max) (format-mono-store . max) (analyze . max) (empty-mono-store . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ui.rkt> . max) (monovariant-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/structs.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/structs.rkt> . max))))"
                  (list 'debug
                        'flow-trace
                        'debug
                        'flow-trace:trace
                        'debug
                        'flow-trace:trace:control
                        'debug
                        'flow-trace:extra
                        ;; 'info
                        ;; 'flow-trace:trace:value

                        'none
                        'flow-trace:trace:local
                        'none
                        'flow-trace:trace:length)
                  #:dump-weaker weaker-path
                  #:dump-stronger stronger-path
                  #:trailing-lines trailing-lines
                  #:print-traces? print-traces?
                  #:show-prefix? show-prefix?
                  ;; #:no-mutate? #t
                  #:diff-effective-Δ? diff-effective-Δ?
                  #:print-Δs? print-Δs?))

(define (tee in path)
  (define-values (pipe-read pipe-write) (make-pipe))
  (thread
   (thunk (call-with-output-file path
            #:exists 'truncate
            (λ (out) (copy-port in out pipe-write)))
          (close-output-port pipe-write)))
  pipe-read)

(define (compare-traces bench-name
                        mutated-module
                        mutation-index
                        weaker-config
                        stronger-config
                        log-topics
                        #:dump-weaker [weaker-path #f]
                        #:dump-stronger [stronger-path #f]
                        #:trailing-lines [trailing-lines 5]
                        #:print-traces? [print-traces? #t]
                        #:show-prefix? [show-prefix? #f]
                        #:no-mutate? [no-mutate? #f]
                        #:diff-effective-Δ? [diff-effective-Δ? #f]
                        #:print-Δs? [print-Δs? #f])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (stronger-pipe-read stronger-pipe-write) (make-pipe))
    (define-values (weaker-pipe-read weaker-pipe-write) (make-pipe))
    (define (spin-up-mutant-with out-port config)
      (parameterize ([current-namespace (make-base-namespace)])
        (thread
         (thunk
          (parameterize [[current-error-port out-port]
                         [current-output-port out-port]]
            (apply with-logging-to-port out-port
                   (thunk
                    ;; Dynamic require inside the dynamic extent of
                    ;; with-logging-to-port so that the flow-trace logger
                    ;; that gets created is a child of the new child created
                    ;; by that form (because we're in a new namespace
                    ;; flow-trace gets instantiated fresh)
                    (define debug-mutant
                      (dynamic-require (module-path-index-resolve debug-mutant-mpi #t)
                                       'debug-mutant))
                    (debug-mutant bench-name
                                  mutated-module
                                  mutation-index
                                  config
                                  #:run? #t
                                  #:suppress-output? #f
                                  #:no-mutate? no-mutate?))
                   ;; Explicitly should not provide logger so a new child
                   ;; logger is created
                   log-topics))
          (close-output-port out-port)))))
    (define weaker-thd
      (spin-up-mutant-with weaker-pipe-write weaker-config))
    (define stronger-thd
      (spin-up-mutant-with stronger-pipe-write stronger-config))

    (define weaker-pipe-read*
      (if weaker-path
          (tee weaker-pipe-read    weaker-path)
          weaker-pipe-read))
    (define stronger-pipe-read*
      (if stronger-path
          (tee stronger-pipe-read    stronger-path)
          stronger-pipe-read))
    (compare-logs weaker-pipe-read* stronger-pipe-read*
                  #:print-after-diff trailing-lines
                  #:print-traces? print-traces?
                  #:show-prefix? show-prefix?
                  #:diff-effective-Δ? diff-effective-Δ?
                  #:print-Δs? print-Δs?))
  ;; upon finding a difference, stop dumping to file system
  (custodian-shutdown-all cust))


(module run-via-shell racket
  (provide run-debug-modules/shell)
  (require "debug-mutant.rkt"
           "debug-traces.rkt")
  (define (run-debug-modules/shell)
    (define benchmarks-dir
      (expand-user-path "~/github_sync/grad/projects/blame-utility/src/gtp-benchmarks"))
    (define max-path (build-path benchmarks-dir "utilities/max.txt"))
    (define weaker-path (build-path benchmarks-dir "utilities/weaker.txt"))
    (define (spin-up-mutant-with mutant-bench-path config log-path)
      (thread
       (thunk
        (debug-mutant "kcfa"
                      "kcfa/untyped/time.rkt"
                      3
                      config
                      #:run? #t
                      #:write-modules-to mutant-bench-path)
        (system
         (format
          "export PLTSTDERR='debug@flow-trace'; racket-7.2 ~a 2>&1 | sed -E 's/debug-(weaker|max)/debug/g' > ~a"
          (path->string (build-path benchmarks-dir
                                    "benchmarks"
                                    mutant-bench-path
                                    "untyped/main.rkt"))
          log-path)))))
    (define weaker-thd
      (spin-up-mutant-with "kcfa-debug-weaker"
                           "#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ai.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ai.rkt> . types) (explore . none) (atom-eval . none) (next . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt> . #hash((make-let . none) (make-lambda . none) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt> . max) (standard-example . types) (make-ref . max) (make-call . max) (new-label . none) (main . types))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/benv.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/benv.rkt> . max) (benv-extend . max) (benv-lookup . none) (benv-extend* . types) (empty-benv . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/denotable.rkt> . #hash((store-update* . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/denotable.rkt> . none) (store-join . max) (d-join . max) (store-update . none) (d-bot . none) (store-lookup . types) (empty-store . types))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/time.rkt> . #hash((time-zero . types) (take* . max) (tick . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/time.rkt> . types) (alloc . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ui.rkt> . #hash((monovariant-value . types) (summarize . max) (format-mono-store . types) (analyze . types) (empty-mono-store . none) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ui.rkt> . max) (monovariant-store . none))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/structs.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/structs.rkt> . types))))"
                           weaker-path))
    (define stronger-thd
      (spin-up-mutant-with "kcfa-debug-max"
                           "#hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ai.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ai.rkt> . max) (explore . max) (atom-eval . max) (next . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt> . #hash((make-let . max) (make-lambda . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt> . max) (standard-example . max) (make-ref . max) (make-call . max) (new-label . max) (main . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/benv.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/benv.rkt> . max) (benv-extend . max) (benv-lookup . max) (benv-extend* . max) (empty-benv . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/denotable.rkt> . #hash((store-update* . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/denotable.rkt> . max) (store-join . max) (d-join . max) (store-update . max) (d-bot . max) (store-lookup . max) (empty-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/time.rkt> . #hash((time-zero . max) (take* . max) (tick . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/time.rkt> . max) (alloc . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ui.rkt> . #hash((monovariant-value . max) (summarize . max) (format-mono-store . max) (analyze . max) (empty-mono-store . max) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/ui.rkt> . max) (monovariant-store . max))) (#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/structs.rkt> . #hash((#<path:/projects/p30818/src/gtp-benchmarks/benchmarks/kcfa/untyped/structs.rkt> . max))))"
                           max-path))
    (thread-wait weaker-thd)
    (thread-wait stronger-thd)

    (call-with-input-file weaker-path
      (λ (weaker-in)
        (call-with-input-file max-path
          (λ (max-in)
            (compare-logs weaker-in max-in)))))))
