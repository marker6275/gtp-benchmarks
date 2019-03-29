#lang racket

(require racket/cmdline)

(define (filter-errors log-in)
  (define (next-line!) (read-line log-in))
  (let loop ()
    (define line (next-line!))
    (define next-line (next-line!))
    (unless (or (eof-object? line) (eof-object? next-line))
      (define line-is-err? (regexp-match? #rx"\\* (error|fatal) \\*"
                                          line))
      (define is-blamed@max-not-bug-err/blamed-random?
        (regexp-match? #rx"blamed region \\('#\\(random.+at max.+not bug"
                       next-line))
      (cond [(and line-is-err?
                  is-blamed@max-not-bug-err/blamed-random?)
             (displayln "--- skipping blamed random ---")
             ;; Skip next lines
             (for ([_ (in-range 7)]) (next-line!))
             (loop)]
            [else
             (displayln line)
             (displayln next-line)
             (loop)]))))

(module+ main
  (define log-path (make-parameter #f))
  (command-line
   #:once-each
   [("-f" "--log-file")
    path
    "Path to log file to filter."
    (log-path path)])
  (if (log-path)
      (call-with-input-file (log-path) filter-errors)
      (filter-errors (current-input-port))))
