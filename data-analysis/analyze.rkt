#lang racket

(require racket/serialize)

(struct mutant (mod index) #:transparent)

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
    (match-define (list _ _ _ mod _ index _ _ _ _) data-list)
    (define this-mutant (mutant mod index))
    (define data-for-mutant (hash-ref data/by-mutant this-mutant
                                      (λ _ (set))))
    (hash-set data/by-mutant
              this-mutant
              (set-add data-for-mutant data-list))))

(define (find-increasing-distances sample-set)
  (for/fold ([results (set)])
            ([sample1 (in-set sample-set)])
    (define increasing-pairs/sample1
      (for/set ([sample2 (in-set sample-set)]
                #:when (and (config-stronger? sample2 sample1)
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
  [{(list-rest _ _ (? number? distance) _)}
   distance]
  [{(list-rest _ _ not-a-distance _)}
   +inf.0])

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


(module+ main
  (define data-dir (make-parameter #f))
  (command-line
   #:once-each
   [("-d" "--data-directory")
    path
    "Directory containing the data."
    (data-dir path)])
  (unless (data-dir)
    (eprintf "Error: data-directory argument is mandatory.~n")
    (exit 1))

  (displayln "Reading and organizing data...")
  (define data (read-data (data-dir)))
  (define data/by-mutant (organize-by-mutant data))
  (displayln "Analyzing for distance increases...")
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
           (eprintf "========== Increasing distance pair ==========~n")
           (for ([sample (in-set samples)]) (displayln sample))
           (eprintf "~n===============~n"))
         (exit 1)]))


(module+ test
  (display-test-results))
