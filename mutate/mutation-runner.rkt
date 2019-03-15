#lang racket

(provide run-with-mutated-module
         report-progress
         make-mutated-module-runner
         make-precision-config-module
         mutate-module
         [struct-out run-status])

(require syntax/parse
         syntax/strip-context
         "mutate.rkt"
         "../ctcs/current-precision-setting.rkt"
         "sandbox-runner.rkt"
         "instrumented-module-runner.rkt"
         "trace.rkt"

         (submod flow-trace/collapsing compressed trace-api))

;; Produce the mutated syntax for the module at the given path
(define (mutate-module module-stx mutation-index)
  (syntax-parse module-stx
    #:datum-literals [module #%module-begin]
    [(module name lang (#%module-begin body ...))
     (define program-stx #'{body ...})
     (match-define (mutated-program program-stx/mutated mutated-id)
       (mutate-program/with-id program-stx mutation-index))
     (values
      (strip-context
       #`(module name lang
           (#%module-begin
            #,@program-stx/mutated)))
      mutated-id)]))

(define (make-precision-config-module precision-config)
  #`(module current-precision-setting racket
      (#%module-begin
       (provide current-precision-config
                precision-configs)
       (define precision-configs '(none types max))
       ;; ll: quoting the hash literal like this should work,
       ;; it makes the syntax '#hash(("path" . setting) ...)
       (define current-precision-config '#,precision-config))))

(define precision-config-module-path "../ctcs/current-precision-setting.rkt")

(define/contract (make-mutated-module-runner main-module
                                             module-to-mutate
                                             other-modules
                                             mutation-index
                                             ctc-precision-config)
  (->i ([main-module path-string?]
        [module-to-mutate path-string?]
        [other-modules (listof path-string?)]
        [mutation-index natural?]
        [ctc-precision-config (hash/c string? symbol?)])
       #:pre (main-module module-to-mutate other-modules)
       (and (not (member main-module other-modules))
            (not (member module-to-mutate other-modules)))

       (values [runner (-> any)]
               [mutated-id symbol?]))

  ;; ll: Ugly hack to get the mutated id out of the instrumentor
  (define mutated-id-box (box #f))

  (define/match (instrument-module path-string stx)
    [{(== precision-config-module-path) _}
     ;; Do not trace the precision-config module, but do set the
     ;; precision settings
     (make-precision-config-module ctc-precision-config)]
    [{(== module-to-mutate) stx}
     #;(trace-module mutated-stx)
     (define-values (mutated-stx mutated-id)
       (mutate-module stx mutation-index))
     ;; ll: see above...
     (set-box! mutated-id-box mutated-id)
     (trace-module module-to-mutate mutated-stx)]
    [{path stx}
     (trace-module path stx)])

  ;; ll: Old code had this as first thing eval'd in namespace. Why?
  ;; (eval '(require "mutate.rkt"))

  (define (setup-namespace! ns)
    ;; Make racket/contract come from the same namespace so that
    ;; we can inspect contract violations thrown inside eval
    (namespace-attach-module (current-namespace)
                             'racket/contract
                             ns)
    ;; Allow working with the returned traces as well
    (namespace-attach-module (current-namespace)
                             '(submod flow-trace/collapsing compressed trace-api)
                             ns))

  (define (before-main-setup! ns)
    (eval '(require (submod flow-trace/collapsing
                              compressed
                              trace-api))
          ns)
    (eval '(current-trace-reset!) ns))

  (define other-modules-to-mutate
    (if (equal? main-module module-to-mutate)
        (list* precision-config-module-path
               other-modules)
        (list* precision-config-module-path
               module-to-mutate
               other-modules)))

  (define runner
    (make-instrumented-module-runner main-module
                                     other-modules-to-mutate
                                     instrument-module
                                     #:setup-namespace setup-namespace!
                                     #:before-main before-main-setup!))
  (define mutated-id (unbox mutated-id-box))

  (values runner mutated-id))

(struct run-status (result-value
                    outcome blamed
                    mutated-module mutated-id index
                    ctc-precision)
  #:transparent)
;; run-status -> bool
(define (index-exceeded? rs)
  (equal? (run-status-outcome rs) 'index-exceeded))


(define report-progress (make-parameter #f))

;; run-status -> void
(define/match (display-run-status rs)
  [{(run-status result-value outcome blamed
                mutated-module mutated-id index
                ctc-precision)}
   (printf "
Run: mutated ~a in module ~a
with precision ~a, index ~a
Status: ~a
Returned: ~a
Blamed: ~a

"
           mutated-id mutated-module
           ctc-precision index
           outcome
           result-value
           blamed)])



(define (run-with-mutated-module main-module
                                 module-to-mutate
                                 other-modules
                                 mutation-index
                                 ctc-precision-config
                                 #:suppress-output? [suppress-output? #t]
                                 #:timeout/s [timeout/s (* 3 60)]
                                 #:memory/gb [memory/gb 3])
  (define (make-status status-sym [blamed #f] [mutated-id #f] [result #f])
    (run-status result
                status-sym
                blamed
                module-to-mutate
                mutated-id
                mutation-index
                ctc-precision-config))
  (with-handlers ([mutation-index-exception?
                   (λ (e) (make-status 'index-exceeded))])
    (define-values (run mutated-id)
      (make-mutated-module-runner main-module
                                  module-to-mutate
                                  other-modules
                                  mutation-index
                                  ctc-precision-config))
    (define ((make-status* status-sym) [blamed #f])
      (make-status status-sym
                   blamed
                   mutated-id
                   (current-trace)))
    (define run/handled
      (λ _
        (with-handlers
          ([exn:fail:contract:blame? (compose
                                      (make-status* 'blamed)
                                      (match-lambda [`(function ,id) id]
                                                    [`(definition ,id) id]
                                                    [other other])
                                      blame-positive
                                      exn:fail:contract:blame-object)]
           [exn:fail:out-of-memory? (λ _ ((make-status* 'oom)))]
           [exn? (make-status* 'crashed)])
          (run)
          ((make-status* 'completed)))))
    (run-with-limits run/handled
                     #:timeout/s timeout/s
                     #:timeout-result (make-status* 'timeout)
                     #:memory/gb memory/gb
                     #:oom-result (make-status* 'oom)
                     #:suppress-output? suppress-output?)))










;; =================================================================
;;                          Old Runners
;; which ...
;; - assume the precision is the same across the whole benchmark
;; - run all the mutants sequentially
;; =================================================================
(module+ old
  (provide run-all-precisions/of-mutant
           run-all-mutants/of-module
           run-all-mutants/with-modules)
  (define (run-all-precisions/of-mutant main-module
                                        module-to-mutate
                                        other-modules
                                        mutation-index
                                        #:suppress-output? [suppress-output? #t]
                                        #:timeout/s [timeout/s (* 3 60)]
                                        #:memory/gb [memory/gb 3])
    (for/list ([ctc-precision precision-configs])
      (when (report-progress)
        (displayln "."))
      (run-with-mutated-module main-module
                               module-to-mutate
                               other-modules
                               mutation-index
                               ctc-precision
                               #:suppress-output? suppress-output?
                               #:timeout/s timeout/s
                               #:memory/gb memory/gb)))

  (define/contract (run-all-mutants/of-module main-module
                                              module-to-mutate
                                              other-modules
                                              #:suppress-output? suppress-output?
                                              #:timeout/s timeout/s
                                              #:memory/gb memory/gb
                                              #:make-result
                                              [make-result identity]
                                              #:start-index
                                              [start-index 0])
    (->i ([main-module path-string?]
          [module-to-mutate path-string?]
          [other-modules (listof path-string?)]
          #:suppress-output? [suppress-output? boolean?]
          #:timeout/s [timeout/s number?]
          #:memory/gb [memory/gb number?])
         (#:make-result [make-result ((listof run-status?) . -> . any/c)]
          #:start-index [start-index natural?])

         #:pre (module-to-mutate main-module other-modules)
         (and (not (member main-module other-modules))
              (not (member module-to-mutate other-modules)))

         [result (listof any/c)])

    (let loop ([index-so-far start-index]
               [results-so-far empty])
      (define results/this-index
        (run-all-precisions/of-mutant main-module
                                      module-to-mutate
                                      other-modules
                                      index-so-far
                                      #:suppress-output? suppress-output?
                                      #:timeout/s timeout/s
                                      #:memory/gb memory/gb))
      (cond [(index-exceeded? (first results/this-index))
             (flatten (reverse results-so-far))]
            [else
             (when (report-progress)
               (displayln "-------------------------")
               (for-each display-run-status results/this-index)
               (displayln "-------------------------"))
             (loop (add1 index-so-far)
                   (cons (make-result results/this-index)
                         results-so-far))])))

  (define/contract (run-all-mutants/with-modules main-module
                                                 mutatable-modules
                                                 #:suppress-output?
                                                 [suppress-output? #t]
                                                 #:timeout/s
                                                 [timeout/s (* 3 60)]
                                                 #:memory/gb
                                                 [memory/gb 3]
                                                 #:make-result
                                                 [make-result identity])
    ([path-string? (listof path-string?)]
     [#:suppress-output? boolean?
      #:timeout/s number?
      #:memory/gb number?
      #:make-result ((listof run-status?) . -> . any/c)]
     . ->* .
     (listof any/c))

    (flatten
     (map (λ (module-to-mutate)
            (define other-modules
              (set-subtract mutatable-modules
                            (list main-module module-to-mutate)))
            (run-all-mutants/of-module main-module
                                       module-to-mutate
                                       other-modules
                                       #:suppress-output? suppress-output?
                                       #:timeout/s timeout/s
                                       #:memory/gb memory/gb
                                       #:make-result make-result))
          mutatable-modules)))

  (module+ test
    (require ruinit)

    (test-begin
      (test-equal?
       (with-output-to-string
         (λ _ (run-with-mutated-module "a.rkt"
                                       "a.rkt"
                                       0
                                       'none
                                       #:suppress-output? #f)))
       "B
(c 5)
(d 1)
(a 0)
(b 1)
4
")

(test-equal?
 (with-output-to-string
   (λ _ (run-with-mutated-module "a.rkt"
                                 "b.rkt"
                                 0
                                 'none
                                 #:suppress-output? #f)))
 "B
(c -1)
(d 1)
(a 1)
(b 1)
-2
")

(test-match
 (run-status 'oom _ _ _ _ _ _)
 (run-with-mutated-module "a.rkt" "a.rkt" 2 'none
                          #:timeout/s 100
                          #:memory/gb 1))

(parameterize ([report-progress #t])
  (test-match
   (list
    (run-status _ 'completed #f "a.rkt" 'a 0 'none)
    (run-status _ 'completed #f "a.rkt" 'a 0 'types)
    (run-status _ 'blamed 'a "a.rkt" 'a 0 'max)
    (run-status _ 'completed #f "a.rkt" 'b 1 'none)
    (run-status _ 'blamed 'b "a.rkt" 'b 1 'types)
    (run-status _ 'blamed 'b "a.rkt" 'b 1 'max)
    (run-status _ 'oom #f "a.rkt" 'foo 2 'none)
    (run-status _ 'oom #f "a.rkt" 'foo 2 'types)
    (run-status _ 'oom #f "a.rkt" 'foo 2 'max)
    (run-status _ 'completed #f "a.rkt" 'foo 3 'none)
    (run-status _ 'completed #f "a.rkt" 'foo 3 'types)
    (run-status _ 'blamed 'foo "a.rkt" 'foo 3 'max)
    (run-status _ 'completed #f "a.rkt" 'foo 4 'none)
    (run-status _ 'completed #f "a.rkt" 'foo 4 'types)
    (run-status _ 'blamed 'foo "a.rkt" 'foo 4 'max)
    (run-status _ 'completed #f "a.rkt" 'foo
                (or 5 6 7 8 9 10 11 12)
                (or 'none 'types 'max))
    ___
    (run-status _ 'completed #f "b.rkt" 'c 0 'none)
    (run-status _ 'blamed 'c "b.rkt" 'c 0 'types)
    (run-status _ 'blamed 'c "b.rkt" 'c 0 'max)
    (run-status _ 'crashed (? exn:fail:contract?) "b.rkt" 'd 1 'none)
    (run-status _ 'blamed 'd "b.rkt" 'd 1 'types)
    (run-status _ 'blamed 'd "b.rkt" 'd 1 'max)
    (run-status _ 'completed #f "b.rkt" 'd 2 'none)
    (run-status _ 'completed #f "b.rkt" 'd 2 'types)
    (run-status _ 'blamed 'd "b.rkt" 'd 2 'max))
   (run-all-mutants/with-modules "a.rkt" '("a.rkt" "b.rkt")))))))


;; for debugging
(module+ debug
  (provide print-mutation)

  (require syntax/modread)
  (define (read-module path)
    (check-module-form
     (with-module-reading-parameterization
       (λ () (with-input-from-file path
               (λ () (port-count-lines! (current-input-port)) (read-syntax)))))
     'ignored path))
  (define (print-mutation module-to-mutate mutation-index)
    (define-values (mutated-program-stx mutated-id)
      (mutate-module (read-module module-to-mutate) mutation-index))
    (printf "--------------------\nMutated: ~a\n\n"
            mutated-id)
    (pretty-print (syntax->datum mutated-program-stx))))
