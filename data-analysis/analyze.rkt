#lang racket

(require racket/serialize
         syntax/parse
         (only-in (submod "../data-collection/mutant-factory.rkt" test)
                  mutant
                  sample-size)
         "../data-collection/benchmarks.rkt"
         "../utilities/read-definitions.rkt")

;; data-list? := (list/c blame-trail-id?
;;                       string?
;;                       distance?
;;                       mod-path? mutated-id? mutation-index?
;;                       outcome? blamed?
;;                       precision-config
;;                       err-msg?)

(define (read-data data-dir)
  (for/fold ([data-so-far empty])
            ([f (in-directory data-dir)])
    (append data-so-far
            (map (compose deserialize (curryr call-with-input-string read))
                 (file->lines f)))))

;; samples? := (set/c data-list?)

;; (listof data-list?) -> (hash/c mutant? samples?)
(define (organize-by-mutant data)
  (for/fold ([data/by-mutant (hash)])
            ([data-list (in-list data)])
    (with-handlers ([exn:misc:match?
                     (λ (e)
                       (error 'organize-by-mutant
                              "Found data missing blame trail id~n~a~n"
                              e))])
      (match-define (list _ _ _ mod _ index _ _ _ _) data-list)
      (define this-mutant (mutant mod index))
      (define data-for-mutant (hash-ref data/by-mutant this-mutant
                                        (λ _ (set))))
      (hash-set data/by-mutant
                this-mutant
                (set-add data-for-mutant data-list)))))

(define (find-increasing-distances sample-set)
  (for/fold ([results (set)])
            ([sample1 (in-set sample-set)])
    (define increasing-pairs/sample1
      (for/set ([sample2 (in-set sample-set)]
                #:when (and (config-stronger? sample2 sample1)
                            (number? (blame-distance sample2))
                            (number? (blame-distance sample1))
                            (> (blame-distance sample2)
                               (blame-distance sample1))))
        (cons sample1 sample2)))
    (set-union results
               increasing-pairs/sample1)))

(module+ test
  (require ruinit)
  (test-begin
    (set-empty? (find-increasing-distances
                 (set (list #f #f 50 (hash 'mod-a (hash 'foo 'max
                                                        'bar 'types)
                                           'mod-b (hash 'baz 'max)))
                      (list #f #f 50 (hash 'mod-a (hash 'foo 'max
                                                        'bar 'max)
                                           'mod-b (hash 'baz 'max))))))
    (set-empty? (find-increasing-distances
                 (set (list #f #f 50 (hash 'mod-a (hash 'foo 'max
                                                        'bar 'types)
                                           'mod-b (hash 'baz 'max)))
                      (list #f #f 25 (hash 'mod-a (hash 'foo 'max
                                                        'bar 'max)
                                           'mod-b (hash 'baz 'max))))))
    (set-empty? (find-increasing-distances
                 (set (list #f #f 50 (hash 'mod-a (hash 'foo 'none
                                                        'bar 'types)
                                           'mod-b (hash 'baz 'types)))
                      (list #f #f 50 (hash 'mod-a (hash 'foo 'types
                                                        'bar 'types)
                                           'mod-b (hash 'baz 'max))))))
    (set-empty? (find-increasing-distances
                 (set (list #f #f 50 (hash 'mod-a (hash 'foo 'none
                                                        'bar 'types)
                                           'mod-b (hash 'baz 'types)))
                      (list #f #f 50 (hash 'mod-a (hash 'foo 'types
                                                        'bar 'types)
                                           'mod-b (hash 'baz 'max)))
                      (list #f #f 25 (hash 'mod-a (hash 'foo 'max
                                                        'bar 'max)
                                           'mod-b (hash 'baz 'max))))))
    (test-equal? (find-increasing-distances
                  (set (list #f #f 50 (hash 'mod-a (hash 'foo 'none
                                                         'bar 'types)
                                            'mod-b (hash 'baz 'types)))
                       (list #f #f 51 (hash 'mod-a (hash 'foo 'types
                                                         'bar 'types)
                                            'mod-b (hash 'baz 'max)))
                       (list #f #f 25 (hash 'mod-a (hash 'foo 'max
                                                         'bar 'max)
                                            'mod-b (hash 'baz 'max)))))
                 (set (cons (list #f #f 50 (hash 'mod-a (hash 'foo 'none
                                                              'bar 'types)
                                                 'mod-b (hash 'baz 'types)))
                            (list #f #f 51 (hash 'mod-a (hash 'foo 'types
                                                              'bar 'types)
                                                 'mod-b (hash 'baz 'max))))))
    (test-equal? (find-increasing-distances
                  (set (list #f #f 50 (hash 'mod-a (hash 'foo 'none
                                                         'bar 'types)
                                            'mod-b (hash 'baz 'types)))
                       (list #f #f 20 (hash 'mod-a (hash 'foo 'types
                                                         'bar 'types)
                                            'mod-b (hash 'baz 'max)))
                       (list #f #f 25 (hash 'mod-a (hash 'foo 'max
                                                         'bar 'max)
                                            'mod-b (hash 'baz 'max)))))
                 (set (cons (list #f #f 20 (hash 'mod-a (hash 'foo 'types
                                                              'bar 'types)
                                                 'mod-b (hash 'baz 'max)))
                            (list #f #f 25 (hash 'mod-a (hash 'foo 'max
                                                              'bar 'max)
                                                 'mod-b (hash 'baz 'max))))))
    (test-equal? (find-increasing-distances
                  (set (list #f #f 50 (hash 'mod-a (hash 'foo 'none
                                                         'bar 'types)
                                            'mod-b (hash 'baz 'types)))
                       (list #f #f 20 (hash 'mod-a (hash 'foo 'types
                                                         'bar 'types)
                                            'mod-b (hash 'baz 'types)))
                       (list #f #f 25 (hash 'mod-a (hash 'foo 'types
                                                         'bar 'types)
                                            'mod-b (hash 'baz 'max)))))
                 (set (cons (list #f #f 20 (hash 'mod-a (hash 'foo 'types
                                                              'bar 'types)
                                                 'mod-b (hash 'baz 'types)))
                            (list #f #f 25 (hash 'mod-a (hash 'foo 'types
                                                              'bar 'types)
                                                 'mod-b (hash 'baz 'max))))))))

(define (analyze-for-increasing-distance data)
  (for/hash ([(mutant samples) (in-hash data)])
    (values mutant
            (find-increasing-distances samples))))

(define/match (blame-distance data-list)
  [{(list-rest _ _ distance _)}
   distance])

(define CONFIG-LEVELS '(none types max))

;; Is the config of `sample-A` stronger than that of `sample-B`?
;;
;; A config is "stronger" than another if every individual level in A
;; is at least as strong as the corresponding level in B.
(define/match (config-stronger? sample-A sample-B)
  [{(list-no-order (? hash? config-A) _ ...)
    (list-no-order (? hash? config-B) _ ...)}
   (for*/and ([(mod mod-config/A) (in-hash config-A)]
              [(region level/A) (in-hash mod-config/A)])
     (define level/B (hash-ref (hash-ref config-B mod) region))
     (>= (index-of CONFIG-LEVELS level/A)
         (index-of CONFIG-LEVELS level/B)))])

(module+ test
  (test-begin
    (config-stronger? (list (hash 'mod-a (hash 'foo 'none)))
                      (list (hash 'mod-a (hash 'foo 'none))))
    (config-stronger? (list (hash 'mod-a (hash 'foo 'types)))
                      (list (hash 'mod-a (hash 'foo 'none))))
    (config-stronger? (list (hash 'mod-a (hash 'foo 'max)))
                      (list (hash 'mod-a (hash 'foo 'none))))
    (config-stronger? (list (hash 'mod-a (hash 'foo 'max)))
                      (list (hash 'mod-a (hash 'foo 'types))))
    (config-stronger? (list (hash 'mod-a (hash 'foo 'max)))
                      (list (hash 'mod-a (hash 'foo 'max))))
    (config-stronger? (list (hash 'mod-a (hash 'foo 'max
                                               'bar 'types)))
                      (list (hash 'mod-a (hash 'foo 'none
                                               'bar 'types))))
    (not (config-stronger? (list (hash 'mod-a (hash 'foo 'max
                                                    'bar 'types)))
                           (list (hash 'mod-a (hash 'foo 'none
                                                    'bar 'max)))))
    (config-stronger? (list (hash 'mod-a (hash 'foo 'max
                                               'bar 'types)
                                  'mod-b (hash 'baz 'max)))
                      (list (hash 'mod-a (hash 'foo 'none
                                               'bar 'types)
                                  'mod-b (hash 'baz 'none))))
    (not (config-stronger? (list (hash 'mod-a (hash 'foo 'max
                                                    'bar 'types)
                                       'mod-b (hash 'baz 'max)))
                           (list (hash 'mod-a (hash 'foo 'none
                                                    'bar 'max)
                                       'mod-b (hash 'baz 'none)))))))


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
                             [(list _ _ _ _ _ _ 'blamed _ _ _) #t]
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

  (printf "
Created ~a mutants, of which ~a were relevant.
Total configurations visited:                            ~a
Blame-trail root count per relevant mutant:              ~a
Average attempted root sample count per relevant mutant: ~a = ~a
Average proportion of root samples that hit blame:       ~a = ~a
"
          total-mutant-count relevant-mutant-count
          total-visit-count
          (sample-size)
          root-sample-count/avg (exact->inexact root-sample-count/avg)
          root-sample-count/avg/hit-blame (exact->inexact
                                           root-sample-count/avg/hit-blame))

  root-sample-count/avg)

(define run-result->blame-trail-id first)

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
