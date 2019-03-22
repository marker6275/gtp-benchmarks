#lang racket

(module test-helper racket
  (require ruinit
           "../utilities/test-env.rkt"
           syntax/parse/define)
  (provide test-begin/with-env)

  (define-test-env (setup-test-env! cleanup-test-env!)
    #:directories ([test-mutant-dir (simplify-path "./test-mutants")]
                   [test-bench (simplify-path "../benchmarks/mutant-test")])
    #:files ([e-path (build-path test-bench "e.rkt")
             #<<HERE
#lang flow-trace

(provide baz)

(define/contract (baz x y)
  (configurable-ctc [max (->i ([x (and/c number? (λ (x) (if (not (even? x)) (not (zero? x)) #t)))]
                               [y number?]) [result number?])]
                    [types (number? number? . -> . number?)])
  (if (even? x)
      y
      (/ y x)))

HERE
]
[d-path (build-path test-bench "d.rkt")
        #<<HERE
#lang flow-trace

(require "e.rkt")

(define/contract (foo x)
  (configurable-ctc [max (->i ([x number?]) [result (x) (=/c (- x))])]
                    [types (number? . -> . number?)])
  (- x))

(foo (baz 0 22))

HERE
]
[mutant0-path "m0.rktd"
              "(\"test\" 102 \"m0.rkt\" foo 0 crashed #f #hash((\"m0.rkt\" . #hash((foo . max) (\"m0.rkt\" . none)))))

"]
[mutant1-path/1 "m11.rktd"
                "(\"test\" 102 \"m1.rkt\" foo 0 blamed foo #hash((\"m1.rkt\" . #hash((foo . max) (\"m1.rkt\" . none)))))

"]
[mutant1-path/2 "m12.rktd"
                "(\"test\" 102 \"m1.rkt\" foo 0 blamed foo #hash((\"m1.rkt\" . #hash((foo . types) (\"m1.rkt\" . none)))))

"]
[mutant2-path "m2.rktd"
              "(\"test\" 102 \"m2.rkt\" foo 0 blamed bar #hash((\"m2.rkt\" . #hash((foo . max) (bar . none) (\"m2.rkt\" . none)))))

"])

#:provide)

(define-simple-macro (test-begin/with-env #:name name
                                          e ...)
  (test-begin
    #:name name
    #:short-circuit
    #:before (setup-test-env!)
    #:after (cleanup-test-env!)
    e ...)))




(require 'test-helper
         ruinit
         (submod "mutant-factory.rkt" test)
         "benchmarks.rkt"
         "../mutate/trace-collect.rkt")

(test-begin/with-env
  #:name test:make-max-bench-config
  (test-equal? (make-max-bench-config (hash-ref benchmarks "mutant-test"))
               (make-config-safe-for-reading
                (hash e-path (hash (path->string e-path) 'max
                                   'baz 'max)
                      d-path (hash (path->string d-path) 'max
                                   'foo 'max)))))

(test-begin
  #:name test:increment-config-precision-for
  (test-equal? (increment-config-precision-for
                (vector 'foo "fake/bench/baz.rkt")
                (hash "fake/bench/baz.rkt" (hash 'bar 'none
                                                 "fake/bench/baz.rkt" 'none
                                                 'foo 'types
                                                 'baz 'max)
                      "fake/bench/bazzle.rkt" (hash 'foo 'none
                                                    "fake/bench/bazzle.rkt" 'max
                                                    'bazzle 'max)))
               (hash "fake/bench/baz.rkt" (hash 'bar 'none
                                                 'foo 'max
                                                 "fake/bench/baz.rkt" 'none
                                                 'baz 'max)
                      "fake/bench/bazzle.rkt" (hash 'foo 'none
                                                    "fake/bench/bazzle.rkt" 'max
                                                    'bazzle 'max)))
  (test-equal? (increment-config-precision-for
                (vector 'foo "fake/bench/bazzle.rkt")
                (hash "fake/bench/baz.rkt" (hash 'bar 'none
                                                 "fake/bench/baz.rkt" 'none
                                                 'foo 'types
                                                 'baz 'max)
                      "fake/bench/bazzle.rkt" (hash 'foo 'none
                                                    "fake/bench/bazzle.rkt" 'none
                                                    'bazzle 'max)))
               (hash "fake/bench/baz.rkt" (hash 'bar 'none
                                                 'foo 'types
                                                 "fake/bench/baz.rkt" 'none
                                                 'baz 'max)
                      "fake/bench/bazzle.rkt" (hash 'foo 'types
                                                    "fake/bench/bazzle.rkt" 'none
                                                    'bazzle 'max))))

(test-begin
  #:name config-at-max-precision-for?
  (not (config-at-max-precision-for?
        (vector 'foo "fake/bench/baz.rkt")
        (hash "fake/bench/baz.rkt" (hash 'bar 'none
                                         'foo 'types
                                         "fake/bench/baz.rkt" 'none
                                         'baz 'max)
              "fake/bench/bazzle.rkt" (hash 'foo 'types
                                            "fake/bench/bazzle.rkt" 'none
                                            'bazzle 'max))))
  (config-at-max-precision-for?
   (vector 'baz "fake/bench/baz.rkt")
   (hash "fake/bench/baz.rkt" (hash 'bar 'none
                                    'foo 'types
                                    "fake/bench/baz.rkt" 'none
                                    'baz 'max)
         "fake/bench/bazzle.rkt" (hash 'foo 'types
                                       "fake/bench/bazzle.rkt" 'none
                                       'bazzle 'max))))

(define dead-e-proc/crashed
  (dead-mutant-process (mutant e-path 0)
                       (hash 'baz 'none
                             e-path 'none)
                       `("test"
                         N/A
                         ,(path->string e-path)
                         'baz
                         0
                         crashed
                         #f
                         ,(hash 'baz 'none
                                (path->string e-path) 'none))
                       0))
(define dead-e-proc/completed
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result `("test"
                         N/A
                         ,(path->string e-path)
                         'baz
                         0
                         completed
                         #f
                         ,(hash 'baz 'none
                                (path->string e-path) 'none))]))
(define dead-e-proc/blame-e
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result `("test"
                         N/A
                         ,(path->string e-path)
                         'baz
                         0
                         blamed
                         ,e-path
                         ,(hash 'baz 'none
                                (path->string e-path) 'none))]))
(define dead-e-proc/blame-baz
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result `("test"
                         N/A
                         ,(path->string e-path)
                         'baz
                         0
                         blamed
                         baz
                         ,(hash 'baz 'none
                                (path->string e-path) 'none))]))
(test-begin/with-env
 #:name dead-process-blame
 (not (try-get-blamed dead-e-proc/crashed))
 (not (try-get-blamed dead-e-proc/completed))
 (test-equal? (try-get-blamed dead-e-proc/blame-e)
              e-path)
 (test-equal? (try-get-blamed dead-e-proc/blame-baz)
              'baz)

 (not (blame-outcome? dead-e-proc/crashed))
 (not (blame-outcome? dead-e-proc/completed))
 (blame-outcome? dead-e-proc/blame-e)
 (blame-outcome? dead-e-proc/blame-baz))

(test-begin/with-env
 #:name test:max-mutation-index-exceeded?

 (test/for/and ([i (in-range 3)])
               (not (max-mutation-index-exceeded? e-path i #t)))
 (max-mutation-index-exceeded? e-path 3 #t)

 (not (max-mutation-index-exceeded? d-path 0 #t))
 (max-mutation-index-exceeded? d-path 1 #t)

 (not (max-mutation-index-exceeded? "forth/untyped/main.rkt"
                                    0))
 (max-mutation-index-exceeded? "forth/untyped/main.rkt"
                               1))

(define mutant0-mod "mutant0.rkt")
(define mutant1-mod "mutant1.rkt")
(define mutant2-mod "mutant2.rkt")
(define mutant0-status (make-parameter 'running))
(define mutant1-status (make-parameter 'running))
(define mutant2-status (make-parameter 'running))
(define (empty-will f _) f)
(define mutant0 (mutant mutant0-mod 0))
(define mutant0-proc (mutant-process mutant0
                                     (hash 'm1 'types
                                           'm2 'none)
                                     mutant0-path
                                     (λ _ (mutant0-status))
                                     empty-will
                                     0))
(define mutant1 (mutant mutant1-mod 1))
(define mutant1-proc/1 (mutant-process mutant1
                                       (hash 'm1 'max
                                             'm2 'none)
                                       mutant1-path/1
                                       (λ _ (mutant1-status))
                                       empty-will
                                       1))
(define mutant1-proc/2 (mutant-process mutant1
                                       (hash 'm1 'types
                                             'm2 'max)
                                       mutant1-path/2
                                       (λ _ (mutant1-status))
                                       empty-will
                                       2))

(define mutant2 (mutant mutant2-mod 2))
(define mutant2-called? (make-parameter #f))
(define (mutant2-will a-factory dead-proc)
  (mutant2-called? #t)
  a-factory)
(define mutant2-proc (mutant-process mutant2
                                     (hash 'm1 'none
                                           'm2 'none)
                                     mutant2-path
                                     (λ _ (mutant2-status))
                                     mutant2-will
                                     3))


(test-begin/with-env
 #:name mutant-results
 (ignore (define mutant1-proc/1-result (call-with-input-file mutant1-path/1
                                         read)))
 (test-equal? (get-mutant-result mutant1-proc/1)
              mutant1-proc/1-result)

 (ignore
  (define aggregate-file (mutant-process-file mutant1-proc/2))
  (define aggregate-file-contents (file->string aggregate-file))
  (define mutant1-proc/1-file (mutant-process-file mutant1-proc/1))
  (define mutant1-proc/1-file-contents (file->string mutant1-proc/1-file))
  (define dead-mutant1-proc/1
    (dead-mutant-process mutant1
                         (mutant-process-config mutant1-proc/1)
                         mutant1-proc/1-result
                         (mutant-process-id mutant1-proc/1)))
  (append-mutant-result!
   (dead-mutant-process-result dead-mutant1-proc/1)
   (aggregate-mutant-result (mutant-process-mutant mutant1-proc/2)
                            (mutant-process-file mutant1-proc/2))))
 (test-equal?
  ;; ll: appending the file contents as opposed to the read-write
  ;; round trip causes an extra newline, so just add that to the expected output
  (string-append (file->string aggregate-file) "\n")
  (string-append aggregate-file-contents
                 mutant1-proc/1-file-contents)))



(parameterize ([mutant2-called? #f])
  (test-begin/with-env
   #:name process-dead-mutant
   (ignore
    (define mutant1-proc/1-result (call-with-input-file mutant1-path/1
                                    read))
    (define mutant2-proc-result (call-with-input-file mutant2-path
                                  read))
    (define aggregate-file (mutant-process-file mutant1-proc/2))
    (define aggregate-file-contents (file->string aggregate-file))
    (define mutant1-aggregate (aggregate-mutant-result mutant1
                                                       aggregate-file))
    (define mutant1-proc/1-file (mutant-process-file mutant1-proc/1))
    (define mutant1-proc/1-file-contents (file->string mutant1-proc/1-file))
    (define mutant2-proc-file (mutant-process-file mutant2-proc))
    (define orig-results (hash mutant1
                               mutant1-aggregate
                               ;; extra garbage not relevant
                               mutant0
                               (aggregate-mutant-result mutant0
                                                        mutant0-path)))
    (define orig-factory (factory "test"
                                  orig-results
                                  (set mutant1-proc/1
                                       mutant2-proc)
                                  2
                                  (set)
                                  5))

    (define new-factory/processed-mutant1-proc/1
      (process-dead-mutant orig-factory mutant1-proc/1))
    (define new-factory/processed-mutant2
      (process-dead-mutant orig-factory mutant2-proc)))

   ;; ll: appending the file contents as opposed to the read-write
   ;; round trip causes an extra newline, so just add that to the expected output
   (test-equal? (string-append (file->string aggregate-file) "\n")
                (string-append aggregate-file-contents
                               mutant1-proc/1-file-contents))
   ;; Due to consolidation, results map is unchanged
   (test-equal? (factory-results new-factory/processed-mutant1-proc/1)
                orig-results)
   ;; But the mutant's file is gone
   (not (file-exists? mutant1-proc/1-file))

   ;; Not so for mutant2, which wasn't in the results hash yet
   (test-equal? (factory-results new-factory/processed-mutant2)
                (hash mutant2
                      ;; The first proc for a mutant to die becomes the
                      ;; aggregate file
                      (aggregate-mutant-result mutant2
                                               (mutant-process-file mutant2-proc))

                      mutant1
                      mutant1-aggregate
                      ;; extra garbage not relevant
                      mutant0
                      (aggregate-mutant-result mutant0
                                               mutant0-path)))
   ;; and its file persists as the aggregate file
   (file-exists? mutant2-proc-file)
   ;; and the will of mutant2 was executed
   (mutant2-called?)))


(parameterize ([data-output-dir test-mutant-dir])
  (test-begin/with-env
   #:name spawn-mutant
   (ignore
    (define orig-factory (factory (bench-info "test" (hash))
                                  (hash)
                                  (set mutant1-proc/1
                                       mutant2-proc)
                                  2
                                  (set)
                                  5))
    (define e-proc-config (hash e-path (hash e-path 'max
                                             'baz 'none)
                                d-path (hash d-path 'none
                                             'foo 'none)))
    (define new-factory (spawn-mutant orig-factory
                                      e-path
                                      0
                                      e-proc-config
                                      empty-will)))
   (test-= (set-count (factory-active-mutants new-factory))
           3)
   (test-= (factory-active-mutant-count new-factory)
           3)
   (test-= (factory-total-mutants-spawned new-factory)
           6)
   ;; spawning a mutant has no effect on the result map
   (test-equal? (factory-results new-factory)
                (factory-results orig-factory))
   ;; or the current bench
   (test-equal? (factory-bench new-factory)
                (factory-bench orig-factory))

   (ignore (match-define (list-no-order (== mutant1-proc/1)
                                        (== mutant2-proc)
                                        e-proc)
             (set->list (factory-active-mutants new-factory))))
   (test-match e-proc
               (mutant-process (mutant (== e-path) 0)
                               (== e-proc-config)
                               _
                               _
                               (== empty-will)
                               _))))



;; Full run test
(parameterize ([data-output-dir test-mutant-dir]
               [process-limit 1])
  (define d&e-mutant-total 4)
  (test-begin/with-env
   #:name test:full-run

   (ignore (displayln 'waiting)
           (displayln `(exists? ,e-path ,(file-exists? e-path)))
           (displayln `(exists? ,d-path ,(file-exists? d-path)))
           ;; (sleep 120)
           (displayln 'starting)
           (define mutant-results (run-all-mutants*configs "mutant-test")))
   (test-= (length (directory-list test-mutant-dir)) d&e-mutant-total)
   (test/for/and ([f (in-directory test-mutant-dir)])
                 (test/not (test-= (file-size f)
                                   0)))))



(display-test-results)
