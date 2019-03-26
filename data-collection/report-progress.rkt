#lang racket/base

(require racket/cmdline
         racket/runtime-path
         "benchmarks.rkt"
         (submod "../mutate/mutation-runner.rkt" debug)
         racket/match
         racket/system
         racket/port
         racket/function
         racket/set
         racket/string
         racket/list)

(define-runtime-path path-to-benchmarks "../benchmarks")
(define grep-exec (find-executable-path "grep"))
(define sort-exec (find-executable-path "sort"))
(define uniq-exec (find-executable-path "uniq"))

(define (get-errors log-path)
  (match-define (list out in _ #f grep-ctl)
    (process*/ports #f #f 'stdout
                    grep-exec
                    "-A" "6"
                    "-E" "ERROR|WARNING"
                    log-path))
  (close-output-port in)
  (grep-ctl 'wait)
  (begin0 (string-split (port->string out) "**********\n")
      (close-input-port out)))

;; grep -E 'snake/untyped/.+ @ [0-9]+[^,][ >]' snake.log > snake-mutants.dat
;; grep -oE 'snake/untyped/.+ @ [0-9]+' snake-mutants.dat > mutants-only.dat
;; sort mutants-only.dat | uniq | less
(define (get-logged-mutants log-path)
  (match-define (list pipeline-out pipeline-in _ pipeline-err ctl)
    (process
     (string-join
      `(,(format "grep -E '[^ ]+/untyped/.+ @ [0-9]+[^,][ >]' ~a" log-path)
        "grep -oE '[^ ]+/untyped/.+ @ [0-9]+'"
        "sort"
        "uniq")
      " | ")))
  (close-output-port pipeline-in)
  (ctl 'wait)
  (begin0 (port->lines pipeline-out)
    (close-input-port pipeline-out)
    (close-input-port pipeline-err)))

(module+ main
  (define bench-name (make-parameter #f))
  (define log-path (make-parameter #f))
  (define force-check-progress (make-parameter #f))
  (command-line
   #:once-each
   [("-b" "--benchmark")
    bench
    "Benchmark name."
    (bench-name bench)]
   [("-l" "--log")
    path
    "Logfile."
    (log-path path)]
   [("-c" "--force-check-progress")
    "Continue checking progress even when there are errors."
    (force-check-progress #t)])
  (unless (and (bench-name) (log-path))
    (eprintf "Error: Both -b and -l arguments are mandatory.~n")
    (exit 1))
  (define bench (hash-ref benchmarks (bench-name)))
  (define bench-mods (cons (benchmark-main bench)
                           (benchmark-others bench)))

  (displayln "Checking for errors...")
  (define errors (get-errors (log-path)))
  (cond [(empty? errors)
         (displayln "No errors encountered.")]
        [else
         (printf "Encountered ~a errors.
==================== Begin errors ====================
"
                 (length errors))
         (for ([err (in-list errors)])
           (printf "------------------------------
~a
------------------------------
"
                   err))
         (printf "
==================================================
")
         (unless (force-check-progress)
           (exit 1))])

  (displayln "Searching modules processed...")
  (define module-mutant-counts
    (for/hash ([mod (in-list bench-mods)])
      (values mod (mutant-count (build-path path-to-benchmarks mod)))))
  (define mutants-logged/text (get-logged-mutants (log-path)))
  (define mutants-logged
    (map (compose rest (curry regexp-match "([^ ]+) @ ([0-9]+)"))
         mutants-logged/text))
  (define mods-started (remove-duplicates (map first mutants-logged)))
  (define mods-completed
    (for/fold ([mods-completed (list)])
              ([logged-mutant mutants-logged])
      (define mod (first logged-mutant))
      (define index (string->number (second logged-mutant)))
      (define mutant-count-for-this-mutant (hash-ref module-mutant-counts mod))
      (if (= index (sub1 mutant-count-for-this-mutant))
          (cons mod mods-completed)
          mods-completed)))
  (define mods-not-started (set-subtract bench-mods mods-started))
  (printf "Started processing mutants of ~v / ~v modules.
    Modules started: ~a
    Not yet started: ~a

"
          #| Completed or near completion (processing last mutant) of ~v / ~v modules.
    Modules completed (or nearly so): ~a |#

          (length mods-started)
          (length bench-mods)
          mods-started
          mods-not-started
          ;; (length mods-completed)
          ;; (length bench-mods)
          ;; mods-completed
          ))

;; lltodo: could also potentially determine how many of the modules are finished
;; by counting the number of samples of each mutant that found blame
