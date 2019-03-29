#lang racket

(require racket/cmdline)

(define (get-blamed line)
  (second (regexp-match #rx"region \\('#\\(([^ ]+) .+at max"
                        line)))
(define (filter-errors log-in blames-to-ignore)
  (define to-ignore-rx
    (format "blamed region \\('#\\((~a).+at max.+not bug"
            (string-join (map regexp-quote blames-to-ignore) ")|("
                         #:before-first "("
                         #:after-last ")")))
  (define (next-line!) (read-line log-in))
  (define (handle-rest-of-chunk! do-with-line)
    (let loop ()
      (define line (next-line!))
      (do-with-line line)
      (unless (or (equal? line "--") (eof-object? line))
        (loop))))
  (let filter-next-chunk ()
    (define line (next-line!))
    (unless (eof-object? line)
      (define line-is-err? (regexp-match? #rx"\\* (error|fatal) \\*"
                                          line))
      (cond [line-is-err?
             (define next-line (next-line!))
             (define is-blamed@max-not-bug-err/blamed-random?
               (regexp-match? to-ignore-rx next-line))
             (cond [is-blamed@max-not-bug-err/blamed-random?
                    (printf "--- skipping irrelevant blame: ~a ---~n"
                            (get-blamed next-line))
                    (handle-rest-of-chunk! void)
                    (filter-next-chunk)]
                   [else
                    (displayln line)
                    (displayln next-line)
                    (handle-rest-of-chunk! displayln)
                    (filter-next-chunk)])]
            [else
             (displayln line)
             (handle-rest-of-chunk! displayln)
             (filter-next-chunk)]))))

(module+ main
  (define log-path (make-parameter #f))
  (define blames-to-ignore (make-parameter empty))
  (command-line
   #:once-each
   [("-f" "--log-file")
    path
    "Path to log file to filter."
    (log-path path)]
   #:args ids
   (blames-to-ignore ids))
  (when (empty? (blames-to-ignore))
    (printf
     "Error: must provide at least one blamed label to filter as arguments~n")
    (exit 1))
  (define filter-blames-to-ignore (curryr filter-errors (blames-to-ignore)))
  (if (log-path)
      (call-with-input-file (log-path) filter-blames-to-ignore)
      (filter-blames-to-ignore (current-input-port))))
