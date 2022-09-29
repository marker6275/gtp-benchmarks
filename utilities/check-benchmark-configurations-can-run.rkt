#lang at-exp rscript

(provide with-fresh-directory!
         copy-files-in!
         is-benchmark-dir?
         resolve-benchmark)

(require racket/random)

(define-runtime-paths
  [default-racket "../../racket/bin/racket"])

(define (is-benchmark-dir? path)
  (match (map ~a (explode-path (simple-form-path path)))
    [(list _ ... "benchmarks" benchmark-name) #t]
    [else #f]))

(define (copy-files-in! source-dir dest-dir [files (directory-list source-dir #:build? #t)])
  (for ([f (in-list files)])
    (copy-file f (build-path dest-dir (basename f)) #t)))

(define (with-fresh-directory! path thunk)
  (when (directory-exists? path)
    (delete-directory/files path))
  (make-directory path)
  (begin0 (thunk)
    (delete-directory/files path)))

(define (sample-half-of l)
  (random-sample l (floor (/ (length l) 2))))

(define (check-benchmark! bench-dir [save-failures-dir-name #f] [racket default-racket])
  (displayln @~a{

                 Checking @bench-dir ...
                 })
  (define unified-dir (build-path bench-dir "unified"))
  (define both-dir (build-path bench-dir "both"))

  (define (run-unified-config/saving-on-failure!)
    (unless (parameterize ([current-directory unified-dir])
              (system* racket "./main.rkt"))
      (when save-failures-dir-name
        (define failure-dir (build-path bench-dir save-failures-dir-name))
        (make-directory* failure-dir)
        (copy-files-in! unified-dir failure-dir)
        (displayln @~a{Failure for @bench-dir saved in @failure-dir}
                   (current-error-port)))
      #f))

  (and (for/and ([t/ut '("typed" "untyped")])
         (displayln t/ut)
         (with-fresh-directory! unified-dir
           (thunk (when (directory-exists? both-dir)
                    (copy-files-in! both-dir unified-dir))
                  (copy-files-in! (build-path bench-dir t/ut) unified-dir)
                  (run-unified-config/saving-on-failure!))))
       (for/and ([i (in-range 10)])
         (displayln @~a{random config @(add1 i) / 10})
         (with-fresh-directory! unified-dir
           (thunk (when (directory-exists? both-dir)
                    (copy-files-in! both-dir unified-dir))
                  (copy-files-in! (build-path bench-dir "typed") unified-dir)
                  (copy-files-in! (build-path bench-dir "untyped")
                                  unified-dir
                                  (sample-half-of (directory-list
                                                   (build-path bench-dir "untyped")
                                                   #:build? #t)))
                  (run-unified-config/saving-on-failure!))))
       (displayln @~a{@bench-dir OK})))

(define (resolve-benchmark path)
  (match (map ~a (explode-path (simple-form-path path)))
    [(list before ... "benchmarks" benchmark-name _ ...)
     (apply build-path (append before (list "benchmarks" benchmark-name)))]
    [else
     (raise-user-error 'check-benchmark
                       @~a{Couldn't infer benchmark dir for current location: @path})]))

(main
 #:arguments ({(hash-table ['auto? auto?]
                           ['save-failures-dir save-failures-dir]
                           ['racket-path racket-path])
               bench-dirs}
              #:once-each
              [("-a" "--auto")
               'auto?
               "Auto-detect the benchmark to which the current-directory belongs."
               #:record]
              [("-s" "--save-failures")
               'save-failures-dir
               "Save failure configurations in the given directory name under each failing benchmark."
               #:collect {"path" take-latest #f}]
              [("-r" "--racket-path")
               'racket-path
               ("Path to the racket executable to run benchmarks with."
                @~a{Default: @default-racket})
               #:collect {"path" take-latest default-racket}]
              #:args benchmark-directories)
 #:check [(or auto?
              (empty? (filter-not is-benchmark-dir? bench-dirs)))
          @~a{
              Benchmark directories must point to the top level dir of a gtp-benchmark.
              These paths don't:
              @(pretty-format (filter-not is-benchmark-dir? bench-dirs))
              }]

 (unless (if auto?
             (check-benchmark! (resolve-benchmark (current-directory))
                               save-failures-dir
                               racket-path)
             (for-each (curryr check-benchmark! save-failures-dir racket-path) bench-dirs))
   (exit 1)))
