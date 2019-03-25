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
   (define crashed? (equal? outcome 'crashed))
   (define blamed-id
     (match* {outcome blame}
       [{'blamed (vector id mod)} id]
       [{'blamed other} (error 'make-trace-distance-results
                               "Got blame that's not a vector: ~v"
                               other)]
       [{_ _} #f]))
   (define message
     (if (equal? outcome 'crashed)
         (try-get-exn-message blame)
         #f))
   (when (and blamed-id (trace-empty? trace))
     (error 'make-trace-distance-results
            "Got a blamed id (~a) but empty trace"
            blamed-id))
   (define distance (trace-distance-between mutated-id
                                            blamed-id
                                            (raw-trace trace)))
   (mutant-outcome bench
                   mutated-module
                   precision
                   outcome
                   (if blamed-id blame #f)
                   mutated-id
                   distance
                   index
                   trace
                   message)])

(struct distance (value) #:transparent)
(struct no-blame () #:transparent)
(struct label-missing (label) #:transparent)
(define (trace-distance-between mutated-label blamed-label trace)
  (cond [(false? blamed-label)
         (no-blame)]
        [(and (hash-has-key? trace mutated-label)
              (hash-has-key? trace blamed-label))
         (distance (- (label-bounds-upper (hash-ref trace blamed-label))
                      (label-bounds-lower (hash-ref trace mutated-label))))]
        [(hash-has-key? trace mutated-label)
         (error 'trace-distance-between
                "Blamed label ~a not found in trace"
                blamed-label)
         (label-missing blamed-label)]
        [else
         (error 'trace-distance-between
                "Mutated label (bug) ~a not found in trace"
                mutated-label)
         (label-missing mutated-label)]))


(define/match (display-mutant-outcome/csv outcome)
  [{(mutant-outcome bench
                    mutated-module
                    precision
                    outcome
                    blamed
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
           distance/repr)])

(define/match (write-mutant-outcome/sexp outcome)
  [{(mutant-outcome bench
                    mutated-module
                    precision
                    outcome
                    blamed
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
                             precision
                             msg)))])

