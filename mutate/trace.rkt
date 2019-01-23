#lang racket

(provide make-instrumented-module-runner
         run-with-tracing)


(require custom-load
         syntax/modread
         (only-in syntax/modresolve [resolve-module-path module-path->path])
         syntax/parse
         syntax/strip-context
         "sandbox-runner.rkt"
         syntax/to-string
         (submod flow-trace/collapsing compressed trace-api)
         ruinit)

(define (module-path-resolve mod-path [load? #f])
  ((current-module-name-resolver) mod-path #f #f load?))

(define (read-module path)
  (check-module-form
   (with-module-reading-parameterization
     (λ () (with-input-from-file path
             (λ () (port-count-lines! (current-input-port)) (read-syntax)))))
   'ignored path))


;; Produce the instrumented syntax for a module to return its
;; execution trace after being run
;; If given the syntax of the module, uses that rather than reading it
;; from the given path
(define/contract (instrument-module module-file-path [module-stx #f])
  ([path?]
   [(or/c #f syntax?)]
   . ->* .
   syntax?)

  (define stx (or module-stx (read-module module-file-path)))
  (syntax-parse stx
    #:datum-literals [module #%module-begin]
    [(module name lang (#%module-begin body ...))
     (strip-context
      #`(module name (submod flow-trace/collapsing compressed)
          (#%module-begin body ...)))]))


(module+test-begin
 (ignore
  (define dummy-mod-path (string->path "/tmp/test.rkt"))
  (define dummy-mod-stx #'(module mod-name racket (#%module-begin a b c))))
 (test-stx=? (instrument-module dummy-mod-path
                                dummy-mod-stx)
             #'(module mod-name (submod flow-trace/collapsing compressed)
                 (#%module-begin a b c)))
 (test-stx=? (instrument-module (string->path "c.rkt"))
             #'(module c (submod flow-trace/collapsing compressed)
                 (#%module-begin (void)))))





(struct instrumented-module
  (path-string module-path file-path containing-directory stx))


(define/contract (make-instrumented-module-runner main-module
                                                  other-modules-to-instrument
                                                  mutated-module
                                                  mutated-module-stx)
  (->i ([main-module path-string?]
        [other-modules-to-instrument (listof path-string?)]
        [mutated-module path-string?]
        [mutated-module-stx syntax?])
       #:pre (main-module mutated-module other-modules-to-instrument)
       (and (not (member main-module other-modules-to-instrument))
            (member mutated-module other-modules-to-instrument))

       [result any/c])

  ;; lltodo: definitely can refactor this stuff
  (define main-module/path-string (path->string (simplify-path main-module)))
  (define main-module/module-path `(file ,main-module/path-string))
  (define main-module/file-path (module-path->path main-module/module-path))
  (define-values (main-module/containing-directory ___1 ___2)
    (split-path (module-path->path main-module/module-path)))
  (define main-module/stx
    (instrument-module main-module/file-path
                       (if (equal? main-module mutated-module)
                           mutated-module-stx
                           #f)))
  (define main-module/instrumented
    (instrumented-module main-module/path-string
                         main-module/module-path
                         main-module/file-path
                         main-module/containing-directory
                         main-module/stx))

  (define modules-to-instrument/strings
    (map (compose path->string simplify-path) other-modules-to-instrument))
  ;; Modules must be loaded in order such that loading one module doesn't
  ;; cause another one to be loaded before it gets instrumented
  (define modules-to-instrument/ordered
    (order-by-dependencies modules-to-instrument/strings))

  (define instrumented-modules
    (for/list ([path-string (in-list modules-to-instrument/ordered)])
      (define module-path `(file ,path-string))
      (define file-path (module-path->path module-path))
      (define-values (module-containing-directory ___1 ___2)
        (split-path file-path))
      (define module-stx (if (equal? file-path mutated-module)
                             mutated-module-stx
                             #f))
      (instrumented-module path-string
                           module-path
                           file-path
                           module-containing-directory
                           (instrument-module file-path module-stx))))

  (define instrumented-modules+main
    (append instrumented-modules
            (list main-module/instrumented)))

  ;; Make racket/contract come from the same namespace so that
  ;; we can inspect contract violations thrown inside eval
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace)
                           'racket/contract
                           ns)
  ;; Allow working with the returned traces as well
  (namespace-attach-module (current-namespace)
                           '(submod flow-trace/collapsing compressed trace-api)
                           ns)

  (define (run)
    (current-trace-reset!)
    (parameterize ([current-load/use-compiled
                    ;; Prevent loading from bytecode to ensure
                    ;; instrumented versions are loaded
                    (make-custom-load/use-compiled
                     #:blacklist (curryr member
                                         (map instrumented-module-file-path
                                              instrumented-modules+main)))]
                   [current-namespace ns])

      (for ([m (in-list instrumented-modules+main)])
        (parameterize
            ([current-load-relative-directory
              (instrumented-module-containing-directory m)]
             [current-module-declare-name
              (module-path-resolve (instrumented-module-module-path m))]
             [current-directory
              (instrumented-module-containing-directory m)])
          (eval (instrumented-module-stx m))))

      ;; Ensure relative load paths work
      (parameterize
          ([current-load-relative-directory main-module/containing-directory]
           [current-directory main-module/containing-directory])
        (eval '(require (submod flow-trace/collapsing
                                compressed
                                trace-api)))
        ;; Eval the main module
        (eval `(require ,main-module/module-path))
        ;; Get the trace
        (eval '(current-trace)))))

  run)

(module+test-begin
 (test-equal? ((make-instrumented-module-runner "d.rkt"
                                                '("e.rkt")
                                                "e.rkt"
                                                (read-module "e.rkt")))
              (hash
               (simplify-path (string->path "./d.rkt"))
               (label-bounds 3 13)
               (simplify-path (string->path "./e.rkt"))
               (label-bounds 0 2)
               'baz
               (label-bounds 1 1)
               'bar
               (label-bounds 6 12)
               'foo
               (label-bounds 4 11)
               '+
               (label-bounds 10 10))))


;; (listof path?) -> (listof path?)
;; Order the given set of modules such that every module in the list
;; only depends on modules earlier in the list than itself.
;;
;; Example: given modules A, B, C, D
;; A depends on B, D
;; B depends on D, C
;; D depends on C
;; Expected result: '(C D B A)
(define (order-by-dependencies modules)
  (->i ([modules (listof path-string?)])
       [result (listof path-string?)]
       #:post (modules result)
       (for/and ([m (in-list result)]
                 [modules-before (in-list (prefixes result))])
         (set-empty? (set-subtract (module-dependencies m modules)
                                   modules-before))))

  (define dependencies-by-module
    (make-hash (map (λ (m) (cons m (module-dependencies m modules)))
                    modules)))
  (define ((all-dependencies-in? modules-seen) m)
    (set-empty? (set-subtract (hash-ref dependencies-by-module m)
                              modules-seen)))
  (let loop ([module-order-so-far empty]
             [modules-remaining modules])
    (cond [(empty? modules-remaining)
           module-order-so-far]
          [else
           (define feasible-modules
             (filter (all-dependencies-in? module-order-so-far)
                     modules-remaining))
           (when (empty? feasible-modules)
             (error 'order-by-dependencies
                    "Module dependency cycle found in ~v"
                    modules))
           (loop (append module-order-so-far feasible-modules)
                 (set-subtract modules-remaining feasible-modules))])))

(module+test-begin
 (test-equal? (order-by-dependencies '("a.rkt" "b.rkt" "c.rkt"))
              '("c.rkt" "b.rkt" "a.rkt"))
 (test-equal? (order-by-dependencies '("../benchmarks/forth/untyped/main.rkt"
                                     "../benchmarks/forth/untyped/command.rkt"
                                     "../benchmarks/forth/untyped/eval.rkt"
                                     "../benchmarks/forth/untyped/stack.rkt"))
              '("../benchmarks/forth/untyped/stack.rkt"
                "../benchmarks/forth/untyped/command.rkt"
                "../benchmarks/forth/untyped/eval.rkt"
                "../benchmarks/forth/untyped/main.rkt")))

;; Produces a list of the list-prefixes of l, including l itself as
;; the last element
(define (prefixes l)
  (append (for/list ([i (in-range (length l))])
            (take l i))
          (list l)))

(module+test-begin
 (test-equal? (prefixes empty)
              '(()))
 (test-equal? (prefixes '(1))
              '(() (1)))
 (test-equal? (prefixes '(1 2))
              '(() (1) (1 2)))
 (test-equal? (prefixes '(1 2 3))
              '(() (1) (1 2) (1 2 3))))

(define/contract (module-dependencies m possible-depends)
  (->i ([m string?]
        [possible-depends (listof string?)])
       [result (possible-depends)
               (and/c (listof string?)
                      (curryr subset? possible-depends))])

  (define module-stx (read-module m))
  (define module-str (syntax->string module-stx))
  (define (module-mentions? other-module)
    (define-values (__1 basename __2) (split-path other-module))
    (string-contains? module-str (path->string basename)))
  (filter module-mentions? (set-subtract possible-depends
                                         (list m))))

(module+test-begin
 ;; Test artifical programs
 (test-equal? (module-dependencies "a.rkt" '("a.rkt" "b.rkt" "c.rkt"))
              '("b.rkt"))
 (test-equal? (module-dependencies "b.rkt" '("a.rkt" "b.rkt" "c.rkt"))
              '("c.rkt"))
 (test-equal? (module-dependencies "c.rkt" '("a.rkt" "b.rkt" "c.rkt"))
              '())

 ;; Test on forth benchmark
 (test-equal? (module-dependencies "../benchmarks/forth/untyped/main.rkt"
                                   '("../benchmarks/forth/untyped/main.rkt"
                                     "../benchmarks/forth/untyped/command.rkt"
                                     "../benchmarks/forth/untyped/eval.rkt"
                                     "../benchmarks/forth/untyped/stack.rkt"))
              '("../benchmarks/forth/untyped/eval.rkt"))
 (test-equal? (module-dependencies "../benchmarks/forth/untyped/command.rkt"
                                   '("../benchmarks/forth/untyped/main.rkt"
                                     "../benchmarks/forth/untyped/command.rkt"
                                     "../benchmarks/forth/untyped/eval.rkt"
                                     "../benchmarks/forth/untyped/stack.rkt"))
              '("../benchmarks/forth/untyped/stack.rkt"))
 (test-equal? (module-dependencies "../benchmarks/forth/untyped/eval.rkt"
                                   '("../benchmarks/forth/untyped/main.rkt"
                                     "../benchmarks/forth/untyped/command.rkt"
                                     "../benchmarks/forth/untyped/eval.rkt"
                                     "../benchmarks/forth/untyped/stack.rkt"))
              '("../benchmarks/forth/untyped/stack.rkt"
                "../benchmarks/forth/untyped/command.rkt"))
 (test-equal? (module-dependencies "../benchmarks/forth/untyped/stack.rkt"
                                   '("../benchmarks/forth/untyped/main.rkt"
                                     "../benchmarks/forth/untyped/command.rkt"
                                     "../benchmarks/forth/untyped/eval.rkt"
                                     "../benchmarks/forth/untyped/stack.rkt"))
              '()))



(define (run-with-tracing main-module
                          modules-to-instrument
                          mutated-module
                          mutated-module-stx
                          #:suppress-output? [suppress-output? #t]
                          #:timeout/s [timeout/s (* 3 60)]
                          #:memory/gb [memory/gb 3])
  (define other-modules-to-instrument (set-subtract modules-to-instrument
                                                    (list main-module)))
  (define run (make-instrumented-module-runner main-module
                                               other-modules-to-instrument
                                               mutated-module
                                               mutated-module-stx))
  (define run/handled
    (λ _
      (with-handlers ([exn? (λ (e) e)])
        (run))))
  (define run-result
    (run-with-limits run/handled
                     #:timeout/s timeout/s
                     #:timeout-result #f
                     #:memory/gb memory/gb
                     #:oom-result #f
                     #:suppress-output? suppress-output?))
  (when (exn? run-result)
    (displayln (format "Warning: unexpected tracing run result ~.a" run-result)
               (current-error-port)))
  ;; Sharing of global trace store means we can just look at it after
  ;; running the program
  (current-trace))

(module+test-begin
 (test-equal? (run-with-tracing "d.rkt"
                                '("e.rkt")
                                "e.rkt"
                                (read-module "e.rkt"))
              (hash
               (simplify-path (string->path "./d.rkt"))
               (label-bounds 3 13)
               (simplify-path (string->path "./e.rkt"))
               (label-bounds 0 2)
               'baz
               (label-bounds 1 1)
               'bar
               (label-bounds 6 12)
               'foo
               (label-bounds 4 11)
               '+
               (label-bounds 10 10))))

(module+ test
  (display-test-results))
