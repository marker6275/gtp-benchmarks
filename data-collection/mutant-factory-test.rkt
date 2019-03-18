#lang racket

(module test-helper racket
  (require ruinit
           (only-in rackunit around)
           syntax/parse/define)
  (provide test-begin/with-env)

  (define-simple-macro (define-test-env (setup:id cleanup:id)
                         [filename:id path:str contents] ...)
    (begin
      (define filename path) ...
      (define (setup)
        (cleanup)
        (display-to-file contents filename) ...)
      (define (cleanup)
        (for ([f (in-list (list filename ...))])
          (when (file-exists? f)
            (delete-file f))))
      (provide filename ...)))
  (define-test-env (setup-test-env! cleanup-test-env!)
    [e-path "e.rkt"
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
[d-path "d.rkt"
        #<<HERE
#lang racket

(require "e.rkt")

(define/contract (foo x)
  any/c
  (+ x 2))

(define/contract (bar x)
  any/c
  (if x
      (foo 42)
      (baz 42 -42)))

(bar 22)
HERE
]
[mutant0-path "m0.rktd"
              "(a b c)\n"]
[mutant1-path "m1.rktd"
              "(d e f)\n"]
[mutant2-path "m2.rktd"
              "(g h i)\n"])

(define-simple-macro (test-begin/with-env e ...)
  (test-begin
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
 (test/for/and ([i (in-range 3)])
               (test/not (max-mutation-index-exceeded? "e.rkt" i #t)))
 (max-mutation-index-exceeded? "e.rkt" 3 #t)
 (test/not
  (max-mutation-index-exceeded? "../benchmarks/forth/untyped/main.rkt"
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
                                       mutant1-path
                                       (λ _ (mutant1-status))))
(define mutant1-proc/2 (mutant-process mutant1
                                       (lattice-point (hash 'm1 'max
                                                            'm2 'none)
                                                      '())
                                       mutant1-path
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
 ;; All mutants have just one process, nothing to consolidate
 (let ([mutant-results (hash mutant0 (set mutant0-proc)
                             mutant1 (set mutant1-proc/1)
                             mutant2 (set mutant2-proc))])
   (test/for/and ([m (in-list mutant0 mutant1 mutant2)])
    (test-equal? (try-consolidate-mutant-results mutant-results m)
                 mutant-results)))
 ;; Two mutants in same set, can consolidate them
 (let ([mutant-results (hash mutant0 (set mutant0-proc mutant1-proc/1)
                             mutant2 (set mutant2-proc))])
   (test/and/message
    [(test-equal? (try-consolidate-mutant-results mutant-results mutant0)
                  (hash
                   mutant0 (set (set-first (hash-ref mutant-results mutant0)))
                   mutant2 (set mutant2-proc)))
     "hash mismatch:"]
    [(file-exists? mutant0-path)
     "Mutant file 0 expected to be preserved, but it wasn't"]
    [(not (file-exists? mutant1-path))
     "mutant1/1 file not deleted"]
    [(test-equal? (file->string mutant0-path)
                  "(a b c)\n(d e f)\n")
     "mutant0 file doesn't contain mutant1/1:"])))

;; maybe-sweep-dead-mutants
(let ([mutant-results (hash mutant0 (set mutant0-proc))]
      ;; duplicate mutant1 to simulate diff configs of same mutant
      [active-mutants (set mutant1-proc/1 mutant1-proc/2
                           mutant2-proc)])
  (test-begin
    (parameterize [[process-limit 100]
                   [mutant1-status 'running]
                   [mutant2-status 'done-ok]]
      ;; `process-limit` well above active count, nothing to sweep
      (call-with-values
       (λ _ (maybe-sweep-dead-mutants mutant-results active-mutants
                                      #:block? #f))
       (λ (r a n)
         (test/and (test-equal? r mutant-results)
                   (test-equal? a active-mutants)
                   (test-= n (set-count active-mutants))))))

    (parameterize [[process-limit 1]
                   [mutant1-status 'running]
                   [mutant2-status 'running]]
      ;; All active mutants running, nothing to sweep
      (call-with-values
       (λ _ (maybe-sweep-dead-mutants mutant-results active-mutants
                                      #:block? #f))
       (λ (r a n)
         (test/and (test-equal? r mutant-results)
                   (test-equal? a active-mutants)
                   (test-= n (set-count active-mutants))))))

    (parameterize [[process-limit 1]
                   [mutant1-status 'done-ok]
                   [mutant2-status 'running]]
      ;; Too many processes running and some of them have completed
      (call-with-values
       (λ _ (maybe-sweep-dead-mutants mutant-results active-mutants
                                      #:block? #f))
       (λ (r a n)
         (test/and
          (test-equal? r
                       (hash mutant0 (set mutant0-proc)
                             mutant1 (set mutant1-proc/1
                                          mutant1-proc/2)))
          (test-equal? a (set mutant2-proc))
          (test-= n 1)))))))


;; add-mutant-result
(test-begin
  (test-equal? (add-mutant-result (hash) mutant1-proc/1)
               (hash mutant1 (set mutant1-proc/1)))
  (test-equal? (add-mutant-result (hash mutant0 (set mutant0-proc))
                                  mutant1-proc/1)
               (hash mutant0 (set mutant0-proc)
                     mutant1 (set mutant1-proc/1)))
  (test-equal? (add-mutant-result (hash mutant0 (set mutant0-proc)
                                        mutant1 (set mutant1-proc/1))
                                  mutant1-proc/2)
               (hash mutant0 (set mutant0-proc)
                     mutant1 (set mutant1-proc/1
                                  mutant1-proc/2))))




(display-test-results)
