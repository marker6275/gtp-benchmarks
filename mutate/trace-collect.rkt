#lang racket

(provide (contract-out [make-trace-distance-results
                        (string?
                         run-status?
                         . -> .
                         mutant-outcome?)])
         display-mutant-outcome/csv)

(require "mutation-runner.rkt"
         "trace.rkt"
         ;; ll: for label-bounds accessors
         (submod flow-trace/collapsing compressed trace-api))

(struct mutant-outcome (bench precision
                              outcome
                              blamed
                              mutated
                              distance
                              mutation-index
                              trace)
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

(define/match (make-trace-distance-results bench _)
  [{bench
    (run-status trace
                  outcome blame
                  mutated-module mutated-id index
                  precision)}
   (define blamed-id blame #;(if (exn? blame) (last-label trace) blame))
   (define distance (trace-distance-between mutated-id
                                            blamed-id
                                            trace))
   (mutant-outcome bench
                   precision
                   outcome
                   blamed-id
                   mutated-id
                   distance
                   index
                   trace)])

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
         (label-missing blamed-label)]
        [else
         (label-missing mutated-label)]))


(define/match (display-mutant-outcome/csv outcome)
  [{(mutant-outcome bench
                    precision
                    outcome
                    blamed
                    mutated
                    maybe-distance
                    index
                    _)}
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
