#lang racket

(provide (contract-out [make-trace-distance-results
                        (string?
                         run-status?
                         . -> .
                         mutant-run?)])
         display-mutant-outcome/csv
         write-mutant-outcome/sexp
         write-mutant-outcome
         make-safe-for-reading
         (struct-out mutant-run)
         (struct-out distance)
         (struct-out no-blame)
         (struct-out label-missing))

(require racket/serialize
         (for-syntax syntax/parse)
         "mutation-runner.rkt"
         "trace.rkt"
         ;; ll: for label-bounds accessors
         (submod flow-trace/collapsing compressed trace-api))

(serializable-struct mutant-run
                     (bench module
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
   (mutant-run bench
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

  (define-match-expander blame-for
    (syntax-parser
      [(_ pat)
       #'(and (? blame?)
              (app blame-positive pat))]))

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
           (vector #f (or (cons (blame-for (== blame-vec)) _)
                          (blame-for (== blame-vec))))
           _ ...)
     'direct/bug!]

    ;; Otherwise, the region whose ctc is being checked is *not*
    ;; blamed. So as long as the violation occurred inside a ctc, it
    ;; is a violation caused indirectly during contract checking e.g.
    ;; triggering a delayed violation, or calling a helper that
    ;; causes a violation
    [(list (vector #f _)
           (vector _ #f) ...
           (vector #f (or (cons (blame-for (not (== blame-vec))) _)
                          (blame-for (not (== blame-vec)))))
           _ ...)
     'indirect]

    [_ 'error:unexpected-shape]))



(serializable-struct distance (value) #:transparent)
(serializable-struct no-blame () #:transparent)
(serializable-struct label-missing (label) #:transparent)
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
  [{(mutant-run bench
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
  [{(mutant-run bench
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

(define (write-mutant-outcome outcome)
  ;; Trace can't be serialized, and we don't want to anyway
  (writeln (serialize (struct-copy mutant-run outcome [trace #f]))))

