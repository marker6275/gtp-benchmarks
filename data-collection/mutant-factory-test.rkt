#lang racket

(module test-helper racket
  (require ruinit
           (only-in rackunit around)
           syntax/parse/define)
  (provide test-begin/with-env)

  (define-simple-macro (define-test-env (setup:id cleanup:id)
                         ({~datum directories}
                          [dir:id dirpath:expr] ...)
                         ({~datum files}
                          [filename:id path:expr contents] ...))
    (begin
      (define dir dirpath) ...
      (define filename path) ...
      (define (setup)
        (cleanup)
        (make-directory dirpath) ...
        (display-to-file contents filename) ...)
      (define (cleanup)
        (for ([f (in-list (list filename ...))])
          (when (file-exists? f)
            (delete-file f)))
        (for ([d (in-list (list dir ...))])
          (when (directory-exists? d)
            (delete-directory/files d))))
      (provide filename ... dir ...)))
  (define-test-env (setup-test-env! cleanup-test-env!)
    (directories [test-mutant-dir "./test-mutants"]
                 [test-bench "../benchmarks/mutant-test"])
    (files
     [e-path (string-append test-bench "/e.rkt")
             #<<HERE
#lang racket

(provide baz)

(define/contract (baz x y)
  any/c
  (if (even? x)
      y
      (/ y x)))

HERE
]
[d-path (string-append test-bench "/d.rkt")
        #<<HERE
#lang racket

(require "e.rkt")

(define/contract (foo x)
  any/c
  (- x))

(foo (baz 0 22))

HERE
]
[mutant0-path "m0.rktd"
              "(a b c)\n"]
[mutant1-path/1 "m11.rktd"
                "(d e f)\n"]
[mutant1-path/2 "m12.rktd"
                "(j k l)\n"]
[mutant2-path "m2.rktd"
              "(g h i)\n"]))

(define-simple-macro (test-begin/with-env name
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
         "lattice.rkt")

;; max-mutation-index-exceeded?
(test-begin/with-env
 test:max-mutation-index-exceeded?

 (test/for/and ([i (in-range 3)])
               (not (max-mutation-index-exceeded? e-path i #t)))
 (max-mutation-index-exceeded? e-path 3 #t)

 (not (max-mutation-index-exceeded? d-path 0 #t))
 (max-mutation-index-exceeded? d-path 1 #t)

 (not (max-mutation-index-exceeded? "../benchmarks/forth/untyped/main.rkt"
                                    0))
 (max-mutation-index-exceeded? "../benchmarks/forth/untyped/main.rkt"
                               1))

(define mutant0-mod "mutant0.rkt")
(define mutant1-mod "mutant1.rkt")
(define mutant2-mod "mutant2.rkt")
(define mutant0-status (make-parameter 'running))
(define mutant1-status (make-parameter 'running))
(define mutant2-status (make-parameter 'running))
(define mutant0 (mutant mutant0-mod 0))
(define mutant0-proc (mutant-process mutant0
                                     (lattice-point (hash 'm1 'types
                                                          'm2 'none)
                                                    '())
                                     mutant0-path
                                     (λ _ (mutant0-status))))
(define mutant1 (mutant mutant1-mod 1))
(define mutant1-proc/1 (mutant-process mutant1
                                       (lattice-point (hash 'm1 'max
                                                            'm2 'none)
                                                      '())
                                       mutant1-path/1
                                       (λ _ (mutant1-status))))
(define mutant1-proc/2 (mutant-process mutant1
                                       (lattice-point (hash 'm1 'types
                                                            'm2 'max)
                                                      '())
                                       mutant1-path/2
                                       (λ _ (mutant1-status))))

(define mutant2 (mutant mutant2-mod 2))
(define mutant2-proc (mutant-process mutant2
                                     (lattice-point (hash 'm1 'none
                                                          'm2 'none)
                                                    '())
                                     mutant2-path
                                     (λ _ (mutant2-status))))

;; try-consolidate-mutant-results
(test-begin/with-env
 test:try-consolidate-mutant-results

 ;; All mutants have just one process, nothing to consolidate
 (let ([mutant-results (hash mutant0 (set mutant0-proc)
                             mutant1 (set mutant1-proc/1)
                             mutant2 (set mutant2-proc))])
   (test/for/and ([m (in-list (list mutant0 mutant1 mutant2))])
                 (test-equal? (try-consolidate-mutant-results mutant-results m)
                              mutant-results)))
 ;; Two mutants in same set, can consolidate them
 (let ([mutant-results (hash mutant1 (set mutant1-proc/1 mutant1-proc/2)
                             mutant2 (set mutant2-proc))])
   (test/and/message
    [(test-equal? (try-consolidate-mutant-results mutant-results mutant1)
                  (hash
                   mutant1 (set (set-first (hash-ref mutant-results mutant1)))
                   mutant2 (set mutant2-proc)))
     "hash mismatch:"]
    [(xor (file-exists? mutant1-path/1) (file-exists? mutant1-path/2))
     "Only one mutant file expected to be preserved"]
    [(if (file-exists? mutant1-path/1)
         (test-equal? (file->string mutant1-path/1)
                      "(d e f)\n(j k l)\n")
         (test-equal? (file->string mutant1-path/2)
                      "(j k l)\n(d e f)\n"))
     "Preserved mutant file doesn't contain the other:"])))


;; add-mutant-result
(test-begin
  (test-equal? (add-mutant-result (hash)
                                  mutant1-proc/1)
               (hash mutant1 (set mutant1-proc/1)))
  (test-equal? (add-mutant-result (hash mutant0 (set mutant0-proc))
                                  mutant1-proc/1)
               (hash mutant0 (set mutant0-proc)
                     mutant1 (set mutant1-proc/1)))
  (test-equal? (add-mutant-result (hash mutant0 (set mutant0-proc)
                                        mutant1 (set mutant1-proc/1))
                                  mutant1-proc/2)
               (hash mutant0 (set mutant0-proc)
                     mutant1 (set mutant1-proc/1 mutant1-proc/2))))


;; sweep-dead-mutants
(let ([mutant-results (hash mutant0 (set mutant0-proc))]
      [active-mutants (set mutant1-proc/1 mutant1-proc/2
                           mutant2-proc)])
  (test-begin/with-env
   test:sweep-dead-mutants

   (parameterize ([mutant1-status 'done-ok]
                  [mutant2-status 'running])
     (call-with-values
      (λ _ (sweep-dead-mutants mutant-results
                               active-mutants
                               (set-count active-mutants)))
      (λ (r a n)
        (test/and/message
         [(test-equal? a (set mutant2-proc))
          "not all mutant1's were swept:"]
         ;; Consolidation is done after every sweep, so the set of
         ;; procs mapped by a mutant that was just sweeped is always 1
         ;;
         ;; Note that either /1 or /2 could be picked, doesn't matter
         [(test/or (test-equal? r
                                (hash mutant0 (set mutant0-proc)
                                      mutant1 (set mutant1-proc/1)))
                   (test-equal? r
                                (hash mutant0 (set mutant0-proc)
                                      mutant1 (set mutant1-proc/2))))
          "not everything is in mutant results:"]
         [(test-= n 1)
          "remaining active mutant count is wrong:"]))))))


;; maybe-sweep-dead-mutants
(let ([mutant-results (hash mutant0 (set mutant0-proc))]
      [active-mutants (set mutant1-proc/1 mutant1-proc/2
                           mutant2-proc)])
  (test-begin/with-env
   test:maybe-sweep-dead-mutants

   (parameterize ([process-limit 100]
                  [mutant1-status 'running]
                  [mutant2-status 'done-ok])
     ;; `process-limit` well above active count, nothing to sweep
     (call-with-values
      (λ _ (maybe-sweep-dead-mutants mutant-results active-mutants
                                     #:block? #f))
      (λ (r a n)
        (test/and (test-equal? r mutant-results)
                  (test-equal? a active-mutants)
                  (test-= n (set-count active-mutants))))))

   (parameterize ([process-limit 1]
                  [mutant1-status 'running]
                  [mutant2-status 'running])
     ;; All active mutants running, nothing to sweep
     (call-with-values
      (λ _ (maybe-sweep-dead-mutants mutant-results active-mutants
                                     #:block? #f))
      (λ (r a n)
        (test/and (test-equal? r mutant-results)
                  (test-equal? a active-mutants)
                  (test-= n (set-count active-mutants))))))

   (parameterize ([process-limit 1]
                  [mutant1-status 'done-ok]
                  [mutant2-status 'running])
     ;; Too many processes running and some of them have completed
     (call-with-values
      (λ _ (maybe-sweep-dead-mutants mutant-results active-mutants
                                     #:block? #f))
      (λ (r a n)
        (test/and/message
         [(test-equal? a (set mutant2-proc))
          "not all mutant1's were swept:"]
         [(test/or (test-equal? r
                                (hash mutant0 (set mutant0-proc)
                                      mutant1 (set mutant1-proc/1)))
                   (test-equal? r
                                (hash mutant0 (set mutant0-proc)
                                      mutant1 (set mutant1-proc/2))))
          "not everything is in mutant results:"]
         [(test-= n 1)
          "remaining active mutant count is wrong:"]))))))



;; Full run test
(parameterize ([data-output-dir test-mutant-dir]
               [process-limit 5])
  (define d&e-mutant-total 4)
  (test-begin/with-env
   test:full-run

   (ignore (define mutant-results (run-all-mutants*configs "mutant-test"))
           (define files (directory-list test-mutant-dir)))
   (test-= (length files) d&e-mutant-total)
   (test/for/and ([f files])
                 (test/not (test-= (file-size (build-path test-mutant-dir f))
                                   0)))))



(display-test-results)
