#lang racket

(provide run-with-mutated-module
         report-progress
         make-mutated-module-runner
         mutate-module
         mutation-index-exception?
         [struct-out run-status])

(require racket/runtime-path
         syntax/parse
         syntax/strip-context
         "mutate.rkt"
         "sandbox-runner.rkt"
         "instrumented-module-runner.rkt"
         "trace.rkt"

         (submod flow-trace/collapsing compressed trace-api))

;; Produce the mutated syntax for the module at the given path
(define (mutate-module module-stx mutation-index)
  (syntax-parse module-stx
    #:datum-literals [module]
    [(module name lang {~and mod-body (mod-begin body ...)})
     #:do [(define program-stx #'{body ...})
           (match-define (mutated-program program-stx/mutated mutated-id)
             (mutate-program/with-id program-stx mutation-index))]
     #:with program/mutated program-stx/mutated
     #:with mutated-mod-stx
     (datum->syntax #'mod-body
                    (syntax-e #'(mod-begin {~@ . program/mutated}))
                    #'mod-body
                    #'mod-body)
     (values (strip-context
              #'(module name lang mutated-mod-stx))
             mutated-id)]))

(define/contract (make-mutated-module-runner main-module
                                             module-to-mutate
                                             other-modules
                                             mutation-index
                                             ctc-precision-config)
  (->i ([main-module path-string?]
        [module-to-mutate path-string?]
        [other-modules (listof path-string?)]
        [mutation-index natural?]
        [ctc-precision-config (hash/c path-string?
                                      (hash/c (or/c symbol? path-string?)
                                              symbol?))])
       #:pre (main-module module-to-mutate other-modules)
       (and (not (member main-module other-modules))
            (not (member module-to-mutate other-modules)))

       (values [runner (-> any)]
               [mutated-id symbol?]))

  ;; ll: Ugly hack to get the mutated id out of the instrumentor
  (define mutated-id-box (box #f))

  (define/match (instrument-module path-string stx)
    [{(== module-to-mutate) stx}
     (define-values (mutated-stx mutated-id)
       (mutate-module stx mutation-index))
     ;; ll: see above...
     (set-box! mutated-id-box mutated-id)
     (trace-module module-to-mutate mutated-stx ctc-precision-config)]
    [{path stx}
     (trace-module path stx ctc-precision-config)])

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
        other-modules
        (list* module-to-mutate
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



(define/contract (run-with-mutated-module main-module
                                          module-to-mutate
                                          other-modules
                                          mutation-index
                                          ctc-precision-config
                                          #:suppress-output? [suppress-output? #t]
                                          #:timeout/s [timeout/s (* 3 60)]
                                          #:memory/gb [memory/gb 3])
  (->i ([main-module path-string?]
        [module-to-mutate path-string?]
        [other-modules (listof path-string?)]
        [mutation-index natural?]
        [ctc-precision-config (hash/c path-string?
                                      (hash/c (or/c symbol? path-string?)
                                              symbol?))])
       (#:suppress-output? [suppress-output? boolean?]
        #:timeout/s [timeout/s number?]
        #:memory/gb [memory/gb number?])

       #:pre (main-module module-to-mutate other-modules)
       (and (not (member main-module other-modules))
            (not (member module-to-mutate other-modules)))

       [result run-status?])

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



(module+ test
  (require ruinit
           "../utilities/test-env.rkt")
  (define-test-env (setup-test-env! cleanup-test-env!)
    #:directories ()
    #:files ([a-path (resolve-path-string "a.rkt")
             #<<HERE
#lang flow-trace

(require "b.rkt")

(define/contract a
  (configurable-ctc
   [max (=/c 1)]
   [types integer?])
  1)
(define/contract b
  (configurable-ctc
   [max (=/c 1)]
   [types positive?])
  1)

(define/contract (foo x y)
  (configurable-ctc
   [max (->i ([x number?]
              [y (x) (and/c number?
                            (>=/c x))])
             [result (x y) (and/c positive?
                                  (=/c (- y x)))])]
   [types (-> number? number? number?)])
  (if #t
      (- y x)
      (+ (for/vector #:length 4294967296 () #f)
         (foo x y))))

(displayln (list 'a a))
(displayln (list 'b b))
(foo d c)
HERE
             ]
[b-path (resolve-path-string "b.rkt")
        #<<HERE
#lang flow-trace

(provide c d)

(require "c.rkt")

(displayln "B")

(define/contract c
  (configurable-ctc
   [max (=/c 5)]
   [types positive?])
  5)
(define/contract d
  (configurable-ctc
   [max (=/c 1)]
   [types number?])
  (if #t 1 "one"))
(displayln (list 'c c))
(displayln (list 'd d))
HERE
        ]
[c-path (resolve-path-string "c.rkt")
        "#lang flow-trace (void)\n"]))
(define none-config
  (hash a-path (hash 'a 'none
                     'b 'none
                     a-path 'none
                     'foo 'none)
        b-path (hash 'c 'none
                     b-path 'none
                     'd 'none)
        c-path (hash c-path 'none)))
(test-begin
  #:before (setup-test-env!)
  #:after (cleanup-test-env!)
    (test-equal?
     (with-output-to-string
       (λ _ (run-with-mutated-module a-path
                                     a-path
                                     (list b-path c-path)
                                     0
                                     none-config
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
   (λ _ (run-with-mutated-module a-path
                                 b-path
                                 (list c-path)
                                 0
                                 none-config
                                 #:suppress-output? #f)))
 "B
(c -1)
(d 1)
(a 1)
(b 1)
-2
")

(test-match
 (run-with-mutated-module a-path a-path
                          (list b-path c-path)
                          2
                          none-config
                          #:timeout/s 100
                          #:memory/gb 1)
 (run-status _ 'oom _ _ _ _ _))))


;; for debugging
(module+ debug
  (provide diff-mutation)

  (require syntax/modread
           ruinit/diff)
  (define (read-module path)
    (check-module-form
     (with-module-reading-parameterization
       (λ () (with-input-from-file path
               (λ () (port-count-lines! (current-input-port)) (read-syntax)))))
     'ignored path))
  (define (diff-mutation module-to-mutate mutation-index)
    (define orig-module-stx (read-module module-to-mutate))
    (define-values (mutated-program-stx mutated-id)
      (mutate-module orig-module-stx mutation-index))
    (printf "--------------------\nMutated: ~a\n" mutated-id)
    (displayln (dumb-diff-lines/string
                (pretty-format (syntax->datum orig-module-stx))
                (pretty-format (syntax->datum mutated-program-stx)))))
  (define (mutant-count module-to-mutate)
    (define module-stx (read-module module-to-mutate))
    (let next-mutant ([index 0])
      (define index-exceeded?
        (with-handlers ([mutation-index-exception? (λ _ #t)])
          (mutate-module module-stx index)
          #f))
      (if index-exceeded?
          index
          (next-mutant (add1 index))))))
