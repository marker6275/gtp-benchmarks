#lang racket

(provide (contract-out [make-trace-distance-results
                        (string?
                         run-status?
                         . -> .
                         mutant-outcome?)])
         display-mutant-outcome/csv
         write-mutant-outcome/sexp
         make-safe-for-reading)

(require racket/serialize
         "mutation-runner.rkt"
         "trace.rkt"
         ;; ll: for label-bounds accessors
         (submod flow-trace/collapsing compressed trace-api))

(struct mutant-outcome (bench module
                              precision
                              outcome
                              blamed
                              blame-type
                              mutated
                              distance
                              mutation-index
                              trace
                              message)
  #:transparent)

(define (last-label trace)
  (for/fold ([max-label #f]
             [max-index -1]
             #:result max-label)
            ([(label bounds) (in-hash trace)])
    (define label-index (label-bounds-upper bounds))
    (if (> label-index max-index)
        (values label label-index)
        (values max-label max-index))))

(define (try-get-exn-message maybe-exn)
  (if (exn? maybe-exn)
      (exn-message maybe-exn)
      maybe-exn))

(define/match (make-trace-distance-results bench _)
  [{bench
    (run-status trace
                outcome blame
                mutated-module mutated-id index
                precision)}
   ;; having the exn in the output is useful for debugging, but the
   ;; distance we should use for crashes is the max
   (define (fail msg . vs)
     (error 'make-trace-distance-results
               "~a
Mutant: ~v @ ~a (~~ ~v) with config
~v
"
               (apply format msg vs)
               mutated-module index mutated-id
               precision))
   (define crashed? (equal? outcome 'crashed))
   (define-values {blamed-id blame-type}
     (match* {outcome blame}
       [{'blamed (cons (and (vector id mod) blame-vec) marks)}
        (values id (categorize-blame blame-vec marks))]
       [{'blamed other}
        (fail "Got blame that's not a vector: ~v" other)]
       [{_ _} (values #f #f)]))
   (define message
     (if (equal? outcome 'crashed)
         (try-get-exn-message blame)
         #f))
   (when (and blamed-id (trace-empty? trace))
     (fail "Got a blamed id (~v) but empty trace." blamed-id))
   (define distance (trace-distance-between mutated-id
                                            blamed-id
                                            (raw-trace trace)
                                            fail))
   (mutant-outcome bench
                   mutated-module
                   precision
                   outcome
                   (if blamed-id (car blame) #f)
                   blame-type
                   mutated-id
                   distance
                   index
                   trace
                   message)])


(define (categorize-blame blame-vec marks)
  (define marks*
    (continuation-mark-set->list* marks
                                  ;; ll: order here affects pattern below
                                  (list
                                   ;; These give the call stack at the
                                   ;; time of violation. Note that the
                                   ;; region whose contract is being
                                   ;; checked will *not* be on the
                                   ;; stack when its ctc gets
                                   ;; violated.
                                   current-label-mark-key
                                   ;; ;; Gives the id of the ctc
                                   ;; ;; currently being checked
                                   ;; current-ctc-continuation-mark-key
                                   contract-continuation-mark-key)))

  ;; Examples: ctc on function 'bar

  ;;;; Case 1: ctc directly violates another fns ctc
  ;; 'blamed
  ;; '(#(bar #<path:/home/lukas/tmp/test7.rkt>) . #<continuation-mark-set>)
  ;; '(#(#f #f (#<blame-yes-swap> . #f))
  ;;   #(#f
  ;;     #f
  ;;     (#<blame-no-swap>
  ;;      .
  ;;      #(#<path:/home/lukas/tmp/test7.rkt> #<path:/home/lukas/tmp/test7.rkt>)))
  ;;   #(bar #f #f)
  ;;   #(#<path:/home/lukas/tmp/test7.rkt> #f #f)
  ;;   #(#f bar #f))

  ;;;; Case 2: ctc causes indirect ctc violation
  ;; 'blamed
  ;; '(#(foo #<path:/home/lukas/tmp/test7.rkt>) . #<continuation-mark-set>)
  ;; '(#(#f #f (#<blame-no-swap> . #(bar #<path:/home/lukas/tmp/test7.rkt>)))
  ;;   #(unbox #f #f)
  ;;   #(#f
  ;;     #f
  ;;     (#<blame-no-swap>
  ;;      .
  ;;      #(#<path:/home/lukas/tmp/test7.rkt> #<path:/home/lukas/tmp/test7.rkt>)))
  ;;   #(bar #f #f)
  ;;   #(#<path:/home/lukas/tmp/test7.rkt> #f #f)
  ;;   #(#f bar #f))
  ;;
  ;;;; Another case 2:
  ;; 'blamed
  ;; '(#(make-let ...))
  ;; '(#(#f #f (#<blame-no-swap> . #(standard-example #<path:/home/lukas/github_sync/grad/projects/blame-utility/src/gtp-benchmarks/benchmarks/kcfa/untyped/main.rkt>)))
  ;;   #(#f #f #<blame-no-swap>)
  ;;   #(#f main #f)
  ;;   #(#f #f #f))

  ;;;; Case 3: ctc calls another fn which causes a violation
  ;; 'blamed
  ;; '(#(foo #<path:/home/lukas/tmp/test7.rkt>) . #<continuation-mark-set>)
  ;; '(#(#f #f (#<blame-yes-swap> . #f))
  ;;   #(foo #f #f)
  ;;   #(#f baz #f)
  ;;   #(foo #f #f)
  ;;   #(foo #f #f)
  ;;   #(#f
  ;;     #f
  ;;     (#<blame-no-swap>
  ;;      .
  ;;      #(#<path:/home/lukas/tmp/test7.rkt> #<path:/home/lukas/tmp/test7.rkt>)))
  ;;   #(bar #f #f)
  ;;   #(#<path:/home/lukas/tmp/test7.rkt> #f #f)
  ;;   #(#f bar #f))

  ;; (displayln marks*)
  ;; (displayln blame-vec)

  ;; (displayln `(first pos ,(blame-positive (car (vector-ref (first marks*) 1)))))
  ;; (displayln `(first neg ,(blame-negative (car (vector-ref (first marks*) 1)))))
  ;; (displayln `(second pos ,(blame-positive (vector-ref (second marks*) 1))))
  ;; (displayln `(second neg ,(blame-negative (vector-ref (second marks*) 1))))

  (match marks*
    ;; If there are no ctc marks on the stack after the first, this
    ;; ctc violation did not occur while checking another ctc
    [(list (vector #f _)
           (vector _ #f) ...)
     'normal]

    ;; Otherwise, the violation should have occurred while checking
    ;; some other ctc...

    ;; The region whose ctc was being checked at the time of violation is blamed
    ;; => the ctc done goofed (directly violated another ctc)
    [(list (vector #f _)
           (vector #f (or (cons (app blame-positive (== blame-vec)) _)
                          (app blame-positive (== blame-vec))))
           _ ...)
     'direct/bug!]

    ;; Otherwise, the region whose ctc is being checked is *not*
    ;; blamed. So as long as the violation occurred inside a ctc, it
    ;; is a violation caused indirectly during contract checking e.g.
    ;; triggering a delayed violation, or calling a helper that
    ;; causes a violation
    [(list (vector #f _) #;(cons (or (app blame-positive (== blame-vec))
                                (app blame-negative (== blame-vec)))
                            _)
           (vector _ #f) ...
           (vector #f (or (cons (app blame-positive (not (== blame-vec))) _)
                          (app blame-positive (not (== blame-vec)))))
           _ ...)
     'indirect]

    [_ 'error:unexpected-shape]))



(struct distance (value) #:transparent)
(struct no-blame () #:transparent)
(struct label-missing (label) #:transparent)
(define (trace-distance-between mutated-label
                                blamed-label
                                trace
                                [report-error (λ args
                                                (apply error
                                                       'trace-distance-between
                                                       args))])
  (cond [(false? blamed-label)
         (no-blame)]
        [(and (hash-has-key? trace mutated-label)
              (hash-has-key? trace blamed-label))
         (distance (- (label-bounds-upper (hash-ref trace blamed-label))
                      (label-bounds-lower (hash-ref trace mutated-label))))]
        [(hash-has-key? trace mutated-label)
         (report-error "Blamed label ~v not found in trace"
                       blamed-label)
         (label-missing blamed-label)]
        [else
         (report-error "Mutated label (bug) ~v not found in trace"
                       mutated-label)
         (label-missing mutated-label)]))


(define/match (display-mutant-outcome/csv outcome)
  [{(mutant-outcome bench
                    mutated-module
                    precision
                    outcome
                    blamed
                    blame-type
                    mutated
                    maybe-distance
                    index
                    _
                    msg)}
   (define distance/repr (match maybe-distance
                           [(distance n) n]
                           [(no-blame) 'N/A]
                           [(label-missing _) 'M/L]))
   (printf "~a, ~a, ~a, ~a, ~a, ~a, ~a~n"
           bench
           precision
           mutated
           index
           outcome
           blamed
           blame-type
           distance/repr)])

(define/match (write-mutant-outcome/sexp outcome)
  [{(mutant-outcome bench
                    mutated-module
                    precision
                    outcome
                    blamed
                    blame-type
                    mutated
                    maybe-distance
                    index
                    _
                    msg)}
   (define distance/repr (match maybe-distance
                           [(distance n) n]
                           [(no-blame) 'N/A]
                           [(label-missing _) 'M/L]))
   (writeln (serialize (list bench
                             distance/repr
                             mutated-module
                             mutated
                             index
                             outcome
                             blamed
                             blame-type
                             precision
                             msg)))])

