#lang racket/base

(provide compare-logs)

(require racket/cmdline
         racket/pretty
         ruinit/diff/diff
         racket/port
         racket/list
         racket/match
         flow-trace/collapsing/trace-rep
         racket/unit
         racket/function
         racket/bool
         racket/contract
         racket/set
         racket/hash)

(define-values/invoke-unit/infer trace-rep-compressed@)

(define ((try-read-from-match pat) str)
  (define m (regexp-match pat str))
  (if m
      (call-with-input-string (second m) read)
      #f))

(define try-read-snoc (try-read-from-match "flow-trace:trace: Snoccing: (.+)$"))
(define try-read-append (try-read-from-match "flow-trace:trace: Appending: (.+)$"))

(define (convert-label-bounds-hash h)
  (for/hash ([(l label-bounds-vec) (in-hash h)])
    (match-define (vector _ lower upper) label-bounds-vec)
    (values l (label-bounds lower upper))))

(define ((make-trace-Δ-reader log1 log2) trace-last in delta-id)
  (printf "\t\t----- ⇓ Δ log ~a [~a] ⇓ -----~n"
          (if (equal? in log1) 1 2)
          delta-id)
  (let continue ()
    (define line (read-line in))
    (if (eof-object? line)
        (values #t #f #f)
        (match* {(try-read-snoc line) (try-read-append line)}
          [{#f #f}
           (printf "~a ~a~n" (if (equal? in log1) "<" ">") line)
           (continue)]
          [{id #f}
           (if (equal? id last)
               (continue)
               (values #f (hash->trace (hash id (label-bounds 0 0))) id))]
          [{#f (list _ ... (== trace-last))}
           (continue)]
          [{#f (and new-trace-list (list _ ... new-trace-last))}
           (values #f new-trace-list new-trace-last)]
          [{#f (vector 'struct:trace (hash-table ((== trace-last) _)) _ _ _)}
           (continue)]
          [{#f (vector 'struct:trace
                       non-singleton-hash _ _ last-id)}
           (values #f
                   (hash->trace (convert-label-bounds-hash non-singleton-hash))
                   last-id)]))))

(define/match (sub-trace? trace/1 trace/2)
  [{(? (negate false?)) (? (negate false?))}
   (define trace-hash/1 (trace-hash trace/1))
   (define trace-hash/2 (trace-hash trace/2))
   (and (hash-keys-subset? trace-hash/1 trace-hash/2)
        (for/and ([(id/1 bounds/1) (in-hash trace-hash/1)])
          (define bounds/2 (hash-ref trace-hash/2 id/1))
          (match-define-values ((label-bounds lower/1 upper/1)
                                (label-bounds lower/2 upper/2))
            (values bounds/1 bounds/2))
          (and (= lower/1 lower/2)
               (<= upper/1 upper/2))))]
  [{_ _} #f])

(module+ test
  (require ruinit)
  (test-begin
    #:name sub-trace?
    (sub-trace? (hash->trace (hash)) (hash->trace (hash)))
    (sub-trace? (hash->trace (hash)) (hash->trace (hash 'a (label-bounds 0 0))))
    (not (sub-trace? (hash->trace (hash 'a (label-bounds 0 0)))
                     (hash->trace (hash))))
    (sub-trace? (hash->trace (hash 'a (label-bounds 0 0)))
                (hash->trace (hash 'a (label-bounds 0 0))))
    (not (sub-trace? (hash->trace (hash 'a (label-bounds 0 0)
                                        'b (label-bounds 1 1)))
                     (hash->trace (hash 'a (label-bounds 1 1)
                                        'b (label-bounds 0 0)))))))

(define (check-consistency trace/1 trace/2)
  (or (sub-trace? trace/1 trace/2)
      (sub-trace? trace/2 trace/1)))

(define/contract (effective-Δ base extended)
  (->i ([base trace?]
        [extended trace?])
       #:pre (base extended) (sub-trace? base extended)
       [result trace?]
       #:post (base extended result)
       (equal? (trace-hash (trace-append base result))
               (trace-hash extended)))

  (define base-hash (trace-hash base))
  (define extended-hash (trace-hash extended))
  (define base-max (trace-length base))

  (hash->trace
   (for/fold ([label-Δs (hash)])
             ([(l b) (in-hash extended-hash)])
     (define extended-upper (label-bounds-upper b))
     (define extended-lower (label-bounds-lower b))
     (cond [(hash-has-key? base-hash l)
            (define base-upper (label-bounds-upper (hash-ref base-hash l)))
            (if (= extended-upper base-upper)
                label-Δs
                (hash-set label-Δs
                          l
                          (label-bounds (if (equal? l (trace-last-label base))
                                            0.2
                                            0.1)
                                        (- extended-upper base-max))))]
           [else
            (hash-set label-Δs
                      l
                      (label-bounds (- extended-lower base-max)
                                    (- extended-upper base-max)))]))))

(module+ test
  (test-begin
    #:name effective-Δ
    (test-equal? (trace-hash
                  (effective-Δ (hash->trace (hash))
                               (hash->trace (hash 'a (label-bounds 0 0)))))
                 (hash 'a (label-bounds 0 0)))
    (test-equal? (trace-hash
                  (effective-Δ (hash->trace (hash 'a (label-bounds 0 0)))
                               (hash->trace (hash 'a (label-bounds 0 2)
                                                  'b (label-bounds 1 1)))))
                 (hash 'b (label-bounds 0 0)
                       'a (label-bounds 0.2 1)))
    (test-equal? (trace-hash
                  (effective-Δ (hash->trace (hash 'a (label-bounds 0 2)
                                                  'b (label-bounds 1 1)))
                               ;; a b a c b
                               (hash->trace (hash 'a (label-bounds 0 2)
                                                  'b (label-bounds 1 4)
                                                  'c (label-bounds 3 3)))))
                 (hash 'c (label-bounds 0 0)
                       'b (label-bounds 0.1 1)))
    (test-equal? (trace-hash
                  (effective-Δ (hash->trace (hash 'a (label-bounds 0 2)
                                                  'b (label-bounds 1 1)))
                               ;; a b a c a b a b
                               (hash->trace (hash 'a (label-bounds 0 6)
                                                  'b (label-bounds 1 7)
                                                  'c (label-bounds 3 3)))))
                 (hash 'c (label-bounds 0 0)
                       'a (label-bounds 0.2 3)
                       'b (label-bounds 0.1 4)))
    (test-equal? (trace-hash
                  (effective-Δ (hash->trace (hash 'a (label-bounds 0 2)
                                                  'b (label-bounds 1 1)))
                               ;; 0 1 2    3 4 5 6 7 8 9 10 11 12
                               ;; a b a -- c a b d a d b a  d  b
                               (hash->trace (hash 'a (label-bounds 0 10)
                                                  'b (label-bounds 1 12)
                                                  'c (label-bounds 3 3)
                                                  'd (label-bounds 6 11)))))
                 (hash 'c (label-bounds 0 0)
                       'a (label-bounds 0.2 7)
                       'b (label-bounds 0.1 9)
                       'd (label-bounds 3 8)))
    (test-equal? (trace-hash
                  (effective-Δ (hash->trace (hash 'a (label-bounds 56 4598)
                                                  'b (label-bounds 59 4597)
                                                  'c (label-bounds 0 4605)))
                               ;; 0 1 2    3 4 5 6 7 8 9 10 11 12
                               ;; a b a -- c a b d a d b a  d  b
                               (hash->trace (hash 'a (label-bounds 56 4606)
                                                  'b (label-bounds 59 4607)
                                                  'c (label-bounds 0 4605)))))
                 (hash 'a (label-bounds 0.1 0)
                       'b (label-bounds 0.1 1)))))

(define (add1! b)
  (set-box! b (add1 (unbox b))))

(define (compare-logs log1 log2
                      #:print-after-diff [continue-lines #f]
                      #:print-traces? [print-traces? #t]
                      #:show-prefix? [show-prefix? #f]
                      #:diff-effective-Δ? [diff-effective-Δ? #f]
                      #:print-Δs? [print-Δs? #f])
  (port-count-lines! log1)
  (port-count-lines! log2)
  (define log1-Δ-count (box 0))
  (define log2-Δ-count (box 0))
  (define read-until-trace-changes (make-trace-Δ-reader log1 log2))

  (define (raise-inconsistency! inconsistent-log
                                last-consistent-state
                                inconsistent-Δ
                                inconsistent-trace
                                other-trace)
    (define-values (inconsistent-log-name
                    inconsistent-log-Δ-count
                    other-log
                    other-log-name
                    other-log-Δ-count)
      (if (equal? inconsistent-log log1)
          (values 1 (unbox log1-Δ-count) log2 2 (unbox log2-Δ-count))
          (values 2 (unbox log2-Δ-count) log1 1 (unbox log1-Δ-count))))
    (match-define-values (next-line-number/1 _ _) (port-next-location log1))
    (match-define-values (next-line-number/2 _ _) (port-next-location log2))
    (define-values (eof? other-trace-Δ other-trace-last-id)
      (read-until-trace-changes (trace-last-label other-trace)
                                other-log
                                other-log-Δ-count))

    (printf "Divergence found around lines ~v / ~v.~n"
            next-line-number/1
            next-line-number/2)
    [printf "
SUMMARY
-------
common prefix ↓ \t        ↓ log ~v effective Δ (part of [~v])
Log ~v: [0]⟶[~v]\t...=====≈≈~~~~ ⟵ log ~v next Δ [~v]
Log ~v: [0]⟶[~v]\t...=====╮
                \t        ╰----
                \t         ↑ log ~v Δ [~v]
"
            other-log-name (sub1 other-log-Δ-count)

            other-log-name (sub1 other-log-Δ-count)
            other-log-name other-log-Δ-count

            inconsistent-log-name (- inconsistent-log-Δ-count 2)
            inconsistent-log-name (sub1 inconsistent-log-Δ-count)]
    (printf "
DESCRIPTION
-----------
Log ~v diverges from log ~v.

Logs agree on a prefix (=====),
~a

After which log ~v is extended by Δ (-----):
~a

while log ~v has the following effective Δ wrt the prefix (≈≈):
 (decimal indices indicate indices unable to be reconstructed)
~a

Diff from which effective Δ was constructed:
~a

The next Δ of log ~v is (~~~~):
~a
"
            inconsistent-log-name other-log-name
            (if show-prefix?
                (pretty-format last-consistent-state)
                (format "<omitted, last-label: ~a>"
                        (trace-last-label last-consistent-state)))

            inconsistent-log-name
            (pretty-format inconsistent-Δ)

            other-log-name
            (pretty-format (effective-Δ last-consistent-state other-trace))

            (if diff-effective-Δ?
                (dumb-diff-lines/string (pretty-format last-consistent-state)
                                        (pretty-format other-trace))
                "<omitted>")

            other-log-name
            (pretty-format other-trace-Δ))

    (when print-traces?
      (printf "Traces before divergence:
Log ~v: ~a
Log ~v: ~a

After divergence:
Log ~v: ~a
"
              inconsistent-log-name (pretty-format last-consistent-state)
              other-log-name (pretty-format other-trace)

              inconsistent-log-name (pretty-format inconsistent-trace)))

    (when continue-lines
      (printf "~nFollowing ~a lines:~n" continue-lines)
      (for ([i (in-range continue-lines)])
        (printf "< ~a~n" (read-line log1)))
      (for ([i (in-range continue-lines)])
        (printf "> ~a~n" (read-line log2))))

    (printf "~n~n [[Divergence found]]: aborting.~n~n"))

  (let loop ([log1-trace empty-trace]
             [log2-trace empty-trace])
    (cond [(sub-trace? log1-trace log2-trace)
           (define log1-last-id (trace-last-label log1-trace))
           (define-values {eof? log1-trace* log1-last-id*}
             (read-until-trace-changes log1-last-id log1
                                       (unbox log1-Δ-count)))
           (when print-Δs?
             (printf "Append: ~a~n"
                     (let ([h (trace-hash log1-trace*)])
                       (if (= (hash-count h) 1)
                           (first (hash-keys h))
                           h))))
           (add1! log1-Δ-count)
           (define log1-trace-appended
             (if (trace? log1-trace*)
                 (trace-append log1-trace log1-trace*)
                 (trace-snoc log1-trace log1-last-id*)))
           (when print-Δs?
             (printf "Current trace: ~a~n"
                     log1-trace-appended))
           (cond [eof?
                  (printf "log1 ended: no inconsistencies found before that")]
                 [(check-consistency log1-trace-appended log2-trace)
                  (loop log1-trace-appended log2-trace)]
                 [else
                  (raise-inconsistency! log1
                                        log1-trace
                                        log1-trace*
                                        log1-trace-appended
                                        log2-trace)])]
          [(sub-trace? log2-trace log1-trace)
           (define log2-last-id (trace-last-label log2-trace))
           (define-values {eof? log2-trace* log2-last-id*}
             (read-until-trace-changes log2-last-id log2
                                       (unbox log2-Δ-count)))
           (when print-Δs?
             (printf "Append: ~a~n"
                     (let ([h (trace-hash log2-trace*)])
                       (if (= (hash-count h) 1)
                           (first (hash-keys h))
                           h))))
           (add1! log2-Δ-count)
           (define log2-trace-appended
             (if (trace? log2-trace*)
                 (trace-append log2-trace log2-trace*)
                 (trace-snoc log2-trace log2-last-id*)))
           (when print-Δs?
             (printf "Current trace: ~a~n"
                     log2-trace-appended))
           (cond [eof?
                  (printf "log1 ended: no inconsistencies found before that")]
                 [(check-consistency log1-trace log2-trace-appended)
                  (loop log1-trace log2-trace-appended)]
                 [else
                  (raise-inconsistency! log2
                                        log2-trace
                                        log2-trace*
                                        log2-trace-appended
                                        log1-trace)])])))

(module+ main
  (define-values (path1 path2)
    (command-line
     #:args (path1 path2)
     (values path1 path2)))
  (call-with-input-file path1
    (λ (log1)
     (call-with-input-file path2
       (λ (log2)
         (compare-logs log1 log2))))))
