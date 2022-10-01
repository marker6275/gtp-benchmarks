#lang at-exp rscript

(require "check-benchmark-configurations-can-run.rkt"
         file/glob)

(define (grep path pattern)
  (regexp-match* pattern (file->string path)))

(define (find-unused-exports bench-dir)
  (define interface-path (build-path bench-dir "both" "type-interface.rkt"))
  (define untyped-dir (build-path bench-dir "untyped"))
  (define clients
    (for/list ([f (in-directory untyped-dir)]
               #:unless (empty? (grep f (regexp-quote @~a{"type-interface.rkt"}))))
      f))
  (define exports
    (map ~a (file->exports interface-path)))
  (define used-exports
    (flatten
     (for/list ([client clients])
       (occurrences-of-in exports client))))
  (set-subtract exports used-exports))

(define (occurrences-of-in names path)
  ; sorting with longest names first ensures that substring-names don't hide
  ; occurrences of the longer name, e.g. searching for both `date` and
  ; `datetime`, the string `datetime` can match `date` first, reporting a match
  ; for `date` instead of `datetime`
  (grep path (disjoin-quoted (sort names > #:key string-length))))

(require "../../blame-evaluation-gt/util/read-module.rkt"
         syntax/parse)
(define-syntax-class r/t/c/p
  #:commit
  #:attributes [(exports 1)]
  (pattern ({~datum require/typed/check/provide}
            lib
            [exports:id . _] ...)))
(define (module->exports stx)
  (syntax-parse stx
    [(module _ _ (#%module-begin {~or* form:r/t/c/p _} ...))
     (for*/list ([es (attribute form.exports)]
                 [id (or es empty)]
                 #:when id)
       (syntax->datum id))]))
(define (file->exports path)
  (module->exports (read-module path)))

(module+ test
  (require ruinit)
  (test-begin
    #:name module->exports
    (test-equal? (module->exports #'(module a racket
                                      (#%mb
                                       (require foo)
                                       (require baz)
                                       (define-type AT Natural)
                                       (require/typed/check/provide "a"
                                                                    [x X]
                                                                    [y (-> Z D)]
                                                                    [n Natural])
                                       (provide AT)
                                       (require/typed/check/provide "b"
                                                                    [g (-> Z D)]))))
                 '(x y n g))
    (test-equal? (module->exports #'(module a racket
                                      (#%mb
                                       (require foo)
                                       (require baz)
                                       (define-type AT Natural)
                                       (provide AT))))
                 '()))
  (require syntax/location)
  ;; Fun test huh?
  (test-begin
    (ignore (define a "FOOBLE")
            (define b "BEEBAR")
            (define c "BEE")) ;; FOOBLE
    (test-equal? (occurrences-of-in (list a b) (second (quote-module-path)))
                 (list a b a))
    (test-equal? (occurrences-of-in (list c a b) (second (quote-module-path)))
                 (list a b c a))))

;; Make a regexp matching any of `strs` literally
(define (disjoin-quoted strs)
  (string-join (map regexp-quote strs) "|"))

(define (check-interface! bench-dir)
  (define unused-exports (find-unused-exports bench-dir))
  (unless (empty? unused-exports)
    (displayln @~a{
                   @bench-dir has unused exports:
                   @pretty-format[unused-exports]

                   })))

(main
 #:arguments ({(hash-table)
               bench-dirs}
              #:args benchmark-directories)
 #:check [(empty? (filter-not is-benchmark-dir? bench-dirs))
          @~a{
              Benchmark directories must point to the top level dir of a gtp-benchmark.
              These paths don't:
              @(pretty-format (filter-not is-benchmark-dir? bench-dirs))
              }]

 (for-each check-interface! bench-dirs))
