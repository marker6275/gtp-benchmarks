#lang at-exp racket

(require racket/serialize
         (only-in (submod "../data-collection/mutant-factory.rkt" test)
                  mutant
                  sample-size)
         "../data-collection/benchmarks.rkt"
         "../utilities/read-definitions.rkt"
         "../mutate/trace-collect.rkt")

(define (read-data data-dir)
  (for/fold ([data-so-far empty])
            ([f (in-directory data-dir)])
    (append data-so-far
            (map (compose deserialize (curryr call-with-input-string read))
                 (file->lines f)))))

;; samples? := (set/c mutant-run?)

;; (listof mutant-run?) -> (hash/c mutant? samples?)
(define (organize-by-mutant data)
  (for/fold ([data/by-mutant (hash)])
            ([data-pair (in-list data)])
    (with-handlers ([exn:misc:match?
                     (λ (e)
                       (error 'organize-by-mutant
                              "Found data missing blame trail id~n~a~n"
                              e))])
      (match-define (cons _
                          (struct* mutant-run ([module mod]
                                               [mutation-index index])))
        data-pair)
      (define this-mutant (mutant mod index))
      (define data-for-mutant (hash-ref data/by-mutant this-mutant
                                        (λ _ (set))))
      (hash-set data/by-mutant
                this-mutant
                (set-add data-for-mutant data-pair)))))

(define (find-increasing-distances sample-set)
  (for/fold ([results (set)])
            ([sample1 (in-set sample-set)])
    (define increasing-pairs/sample1
      (for/set ([sample2 (in-set sample-set)]
                #:when
                (match* {sample1 sample2}
                  [{(cons _ (struct* mutant-run ([blame-type 'normal]
                                                 [distance (distance dist1)]
                                                 [precision config1])))
                    (cons _ (struct* mutant-run ([blame-type 'normal]
                                                 [distance (distance dist2)]
                                                 [precision config2])))}
                   (and (config-stronger? config2 config1)
                        (> dist2 dist1))]
                  [{_ _} #f]))
        (cons sample1 sample2)))
    (set-union results
               increasing-pairs/sample1)))

(module+ test
  (require ruinit)
  (define (make-run-result dist config)
    (cons #f (mutant-run #f #f config #f #f 'normal #f (distance dist) #f #f #f)))
  (test-begin
    (set-empty? (find-increasing-distances
                 (set (make-run-result 50
                                       (hash 'mod-a (hash 'foo 'max
                                                        'bar 'types)
                                              'mod-b (hash 'baz 'max)))
                      (make-run-result 50
                                       (hash 'mod-a (hash 'foo 'max
                                                        'bar 'max)
                                              'mod-b (hash 'baz 'max))))))
    (set-empty? (find-increasing-distances
                 (set (make-run-result 50
                                       (hash 'mod-a (hash 'foo 'max
                                                        'bar 'types)
                                              'mod-b (hash 'baz 'max)))
                      (make-run-result 25
                                       (hash 'mod-a (hash 'foo 'max
                                                        'bar 'max)
                                              'mod-b (hash 'baz 'max))))))
    (set-empty? (find-increasing-distances
                 (set (make-run-result 50
                                       (hash 'mod-a (hash 'foo 'none
                                                        'bar 'types)
                                              'mod-b (hash 'baz 'types)))
                      (make-run-result 50
                                       (hash 'mod-a (hash 'foo 'types
                                                        'bar 'types)
                                              'mod-b (hash 'baz 'max))))))
    (set-empty? (find-increasing-distances
                 (set (make-run-result 50
                                       (hash 'mod-a (hash 'foo 'none
                                                        'bar 'types)
                                              'mod-b (hash 'baz 'types)))
                      (make-run-result 50
                                       (hash 'mod-a (hash 'foo 'types
                                                        'bar 'types)
                                              'mod-b (hash 'baz 'max)))
                      (make-run-result 25
                                       (hash 'mod-a (hash 'foo 'max
                                                        'bar 'max)
                                              'mod-b (hash 'baz 'max))))))
    (test-equal? (find-increasing-distances
                  (set (make-run-result 50
                                        (hash 'mod-a (hash 'foo 'none
                                                           'bar 'types)
                                              'mod-b (hash 'baz 'types)))
                       (make-run-result 51
                                        (hash 'mod-a (hash 'foo 'types
                                                           'bar 'types)
                                              'mod-b (hash 'baz 'max)))
                       (make-run-result 25
                                        (hash 'mod-a (hash 'foo 'max
                                                           'bar 'max)
                                              'mod-b (hash 'baz 'max)))))
                 (set (cons (make-run-result 50
                                             (hash 'mod-a (hash 'foo 'none
                                                                'bar 'types)
                                                   'mod-b (hash 'baz 'types)))
                            (make-run-result 51
                                             (hash 'mod-a (hash 'foo 'types
                                                                'bar 'types)
                                                   'mod-b (hash 'baz 'max))))))
    (test-equal? (find-increasing-distances
                  (set (make-run-result 50
                                        (hash 'mod-a (hash 'foo 'none
                                                         'bar 'types)
                                               'mod-b (hash 'baz 'types)))
                       (make-run-result 20
                                        (hash 'mod-a (hash 'foo 'types
                                                         'bar 'types)
                                               'mod-b (hash 'baz 'max)))
                       (make-run-result 25
                                        (hash 'mod-a (hash 'foo 'max
                                                         'bar 'max)
                                               'mod-b (hash 'baz 'max)))))
                 (set (cons (make-run-result 20
                                             (hash 'mod-a (hash 'foo 'types
                                                              'bar 'types)
                                                    'mod-b (hash 'baz 'max)))
                            (make-run-result 25
                                             (hash 'mod-a (hash 'foo 'max
                                                              'bar 'max)
                                                    'mod-b (hash 'baz 'max))))))
    (test-equal? (find-increasing-distances
                  (set (make-run-result 50
                                        (hash 'mod-a (hash 'foo 'none
                                                         'bar 'types)
                                               'mod-b (hash 'baz 'types)))
                       (make-run-result 20
                                        (hash 'mod-a (hash 'foo 'types
                                                         'bar 'types)
                                               'mod-b (hash 'baz 'types)))
                       (make-run-result 25
                                        (hash 'mod-a (hash 'foo 'types
                                                         'bar 'types)
                                               'mod-b (hash 'baz 'max)))))
                 (set (cons (make-run-result 20
                                             (hash 'mod-a (hash 'foo 'types
                                                              'bar 'types)
                                                    'mod-b (hash 'baz 'types)))
                            (make-run-result 25
                                             (hash 'mod-a (hash 'foo 'types
                                                              'bar 'types)
                                                    'mod-b (hash 'baz 'max))))))))

(define (analyze-for-increasing-distance data)
  (for/hash ([(mutant samples) (in-hash data)])
    (values mutant
            (find-increasing-distances samples))))

(define CONFIG-LEVELS '(none types max))

;; Is the config of `sample-A` stronger than that of `sample-B`?
;;
;; A config is "stronger" than another if every individual level in A
;; is at least as strong as the corresponding level in B.
(define (config-stronger? config-A config-B)
  (for*/and ([(mod mod-config/A) (in-hash config-A)]
             [(region level/A) (in-hash mod-config/A)])
    (define level/B (hash-ref (hash-ref config-B mod) region))
    (>= (index-of CONFIG-LEVELS level/A)
        (index-of CONFIG-LEVELS level/B))))

(module+ test
  (test-begin
    (config-stronger? (hash 'mod-a (hash 'foo 'none))
                      (hash 'mod-a (hash 'foo 'none)))
    (config-stronger? (hash 'mod-a (hash 'foo 'types))
                      (hash 'mod-a (hash 'foo 'none)))
    (config-stronger? (hash 'mod-a (hash 'foo 'max))
                      (hash 'mod-a (hash 'foo 'none)))
    (config-stronger? (hash 'mod-a (hash 'foo 'max))
                      (hash 'mod-a (hash 'foo 'types)))
    (config-stronger? (hash 'mod-a (hash 'foo 'max))
                      (hash 'mod-a (hash 'foo 'max)))
    (config-stronger? (hash 'mod-a (hash 'foo 'max
                                         'bar 'types))
                      (hash 'mod-a (hash 'foo 'none
                                         'bar 'types)))
    (not (config-stronger? (hash 'mod-a (hash 'foo 'max
                                              'bar 'types))
                           (hash 'mod-a (hash 'foo 'none
                                              'bar 'max))))
    (config-stronger? (hash 'mod-a (hash 'foo 'max
                                         'bar 'types)
                            'mod-b (hash 'baz 'max))
                      (hash 'mod-a (hash 'foo 'none
                                         'bar 'types)
                            'mod-b (hash 'baz 'none)))
    (not (config-stronger? (hash 'mod-a (hash 'foo 'max
                                              'bar 'types)
                                 'mod-b (hash 'baz 'max))
                           (hash 'mod-a (hash 'foo 'none
                                              'bar 'max)
                                 'mod-b (hash 'baz 'none))))))


(define (report-distance-increases data/by-mutant)
  (define increasing-distance-pairs/by-mutant
    (analyze-for-increasing-distance data/by-mutant))
  (define increasing-distance-pairs
    (filter (negate set-empty?)
            (hash-values increasing-distance-pairs/by-mutant)))
  (cond [(empty? increasing-distance-pairs)
         (displayln "No increasing distance pairs found.")]
        [else
         (displayln "Increasing distance pairs found:")
         (for ([samples (in-list increasing-distance-pairs)])
           (eprintf "========== Increasing distance pairs ==========~n")
           (for ([sample (in-set samples)]) (printf "~s~n" sample))
           (eprintf "~n===============~n"))]))



(define (report-mutant&sample-info data/by-mutant)
  (define total-mutant-count (hash-count data/by-mutant))
  (define-values (relevant-mutant-count
                  config-visit-counts/by-mutant
                  root-sample-counts/by-mutant)
    (for/fold ([relevant-mutant-count 0]
               [config-visit-counts/by-mutant (hash)]
               [root-sample-counts/by-mutant (hash)])
              ([(mutant runs) (in-hash data/by-mutant)])
      (define mutant-relevant?
        (for/first ([run (in-set runs)]
                    #:when (match run
                             [(cons _ (struct* mutant-run ([outcome 'blamed]))) #t]
                             [_ #f]))
          #t))
      (define root-sample-count
        (length (remove-duplicates (set-map runs run-result->blame-trail-id))))
      (values (if mutant-relevant?
                  (add1 relevant-mutant-count)
                  relevant-mutant-count)
              (hash-set config-visit-counts/by-mutant
                        mutant
                        (set-count runs))
              (hash-set root-sample-counts/by-mutant
                        mutant
                        root-sample-count))))
  (define total-visit-count
    (apply + (hash-values config-visit-counts/by-mutant)))
  (define total-root-sample-count
    (apply + (hash-values root-sample-counts/by-mutant)))
  (define root-sample-count/less-irrelevant-mutants
    (- total-root-sample-count (- total-mutant-count relevant-mutant-count)))
  (define root-sample-count/avg (/ root-sample-count/less-irrelevant-mutants
                                   relevant-mutant-count))
  (define root-sample-count/avg/hit-blame
    (/ (sample-size) root-sample-count/avg))

  (match-define
    (list normal-blame-count indirect-blame-count buggy-blame-count err-count)
    (for/fold ([counts '(0 0 0 0)])
              ([(mutant samples) (in-hash data/by-mutant)]
               #:when #t
               [run (in-set samples)])
      (match (mutant-run-blame-type (cdr run))
        [(? symbol? type)
         (list-update counts
                   (index-of '(normal indirect direct/bug! error:unexpected-shape)
                             type)
                   add1)]
        [#f counts])))

  (displayln
   @~a{

Created @total-mutant-count mutants, of which @relevant-mutant-count were relevant.
Total configurations visited:                            @total-visit-count
Blame-trail root count per relevant mutant:              @(sample-size)
Average attempted root sample count per relevant mutant: @root-sample-count/avg = @(exact->inexact root-sample-count/avg)
Average proportion of root samples that hit blame:       @root-sample-count/avg/hit-blame = @(exact->inexact root-sample-count/avg/hit-blame)

Blame categories
(regular violation)                   Normal:            @normal-blame-count
(indirect violation in ctc checking)  Indirect:          @indirect-blame-count
(direct violation by ctc code)        Direct/bug:        @buggy-blame-count
(blame in unexpected context)         Unknown:           @err-count
})

  root-sample-count/avg)

(define run-result->blame-trail-id car)

(define (report-lattice-size bench-name avg-samples-per-mutant)
  (define total-region-count (count-top-level-defs bench-name))
  (define lattice-size (expt 3 total-region-count))
  (printf "
Total lattice size for benchmark:                                ~a
Average proportion of lattice explored for each relevant mutant: ~a
"
          (format-big-number lattice-size)
          (exact->inexact (/ avg-samples-per-mutant lattice-size))))

(define (count-top-level-defs bench-name)
  (length (append* (hash-values (top-level-defs/by-module bench-name)))))

(define (format-big-number big-number)
  (define number-string (number->string big-number))
  (list->string
   (for/fold ([fancy-digits empty])
             ([digit (in-list (reverse (string->list number-string)))]
              [i (in-naturals)])
     (if (and (not (zero? i)) (zero? (modulo i 3)))
         (list* digit #\, fancy-digits)
         (cons digit fancy-digits)))))


(module+ main
  (define data-dir (make-parameter #f))
  (define bench-name (make-parameter #f))
  (command-line
   #:once-each
   [("-d" "--data-directory")
    path
    "Directory containing the data."
    (data-dir path)]
   [("-b" "--benchmark-name")
    name
    "Benchmark for which the data was collected."
    (bench-name name)])
  (unless (and (data-dir) (bench-name))
    (eprintf "Error: arguments -d, -b are mandatory.~n")
    (exit 1))

  (printf "========== Analysis of data for benchmark ~a ==========~n"
          (bench-name))
  (displayln "Reading and organizing data...")
  (define data (read-data (data-dir)))
  (define data/by-mutant (organize-by-mutant data))

  (define avg-root-samples-per-mutant
    (report-mutant&sample-info data/by-mutant))

  (report-lattice-size (bench-name) avg-root-samples-per-mutant)

  (newline)
  (displayln "Analyzing for distance increases...")
  (report-distance-increases data/by-mutant))


(module+ test
  (display-test-results))
