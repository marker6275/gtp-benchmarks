#lang racket

(provide (contract-out [make-lattice
                        (point?
                         (point? . -> . (set/c point?))
                         . -> .
                         (set/c lattice-point?))])
         (struct-out lattice-point))

(module+ test
  (require ruinit))


(define point? any/c)
(define path? (listof point?))
;; lattice-point = point? (set/c path?)
(struct lattice-point (value paths-to) #:transparent)

(define (make-lattice start parents-of)
  (define h (make-lattice-hash start parents-of))
  (for/set ([(point paths) h])
    (lattice-point point paths)))





(module+ test
  (struct mod-config (mod level) #:transparent)
  (define inc (match-lambda [(mod-config m 'T) (mod-config m 'M)]
                            [(mod-config m 'N) (mod-config m 'T)]))
  (define max? (match-lambda [(mod-config m 'M) #t]
                             [_ #f]))
  (define (parents-of point)
    (for/fold ([parents (set)])
              ([mod-config point])
      (define other-mods (set-remove point mod-config))
      (cond [(max? mod-config) parents]
            [else
             (define bench-config-with-mod+1
               (set-add other-mods (inc mod-config)))
             (set-add parents bench-config-with-mod+1)])))

  (define (parse-config config-as-list)
    (for/set ([mod-as-list config-as-list])
      (mod-config (first mod-as-list) (second mod-as-list))))
  (define (parse-paths-hash p)
      (for/hash ([(c path-list) p])
        (values (parse-config c)
                (list->set (map (curry map parse-config) path-list)))))
  (define-test (test-path-maps=? actual expected)
    (unless (equal? actual expected)
      (fail "Path maps differ.\nActual:\n~a\nExpected:\n~a\n"
            (pretty-format actual)
            (pretty-format expected)))))


(define/contract (make-lattice-hash start parents-of)
  (point?
   (point? . -> . (set/c point?))
   . -> .
   (hash/c point? (set/c path?)))

  (let loop (;; hash of point? -> (set/c path?)
             [paths (hash start (set '()))]
             ;; set of point?
             [seen (set)]
             ;; list of point?
             [to-process (list start)])
    (cond [(set-empty? to-process)
           paths]

          [else
           (define bench (first to-process))
           (define parents (parents-of bench))
           (define parents/unseen (set->list (set-subtract parents seen)))
           (define paths+bench->parents
             (add-paths paths bench parents))
           (loop paths+bench->parents
                 (set-add seen bench)
                 (append (rest to-process)
                         parents/unseen))])))

(module+ test
  (define-test (test-lattice start expected)
    (define start/parsed (parse-config start))
    (define expected/parsed (parse-paths-hash expected))
    (test-path-maps=? (make-lattice-hash start/parsed parents-of)
                      expected/parsed))
  (test-begin
    (test-lattice
     '[(A N)]
     (hash '[(A N)]
           '(())

           '[(A T)]
           '(([(A N)]))

           '[(A M)]
           '(([(A T)] [(A N)]))))

    (test-lattice
     '[(A N) (B N)]
     (hash '[(A N) (B N)]
           '(())

           '[(A T) (B N)]
           '(([(A N) (B N)]))

           '[(A N) (B T)]
           '(([(A N) (B N)]))

           '[(A M) (B N)]
           '(([(A T) (B N)] [(A N) (B N)]))

           '[(A N) (B M)]
           '(([(A N) (B T)] [(A N) (B N)]))

           '[(A T) (B T)]
           '(([(A T) (B N)] [(A N) (B N)])
             ([(A N) (B T)] [(A N) (B N)]))

           '[(A M) (B T)]
           '(([(A M) (B N)] [(A T) (B N)] [(A N) (B N)])
             ([(A T) (B T)] [(A T) (B N)] [(A N) (B N)])
             ([(A T) (B T)] [(A N) (B T)] [(A N) (B N)]))

           '[(A T) (B M)]
           '(([(A N) (B M)] [(A N) (B T)] [(A N) (B N)])
             ([(A T) (B T)] [(A T) (B N)] [(A N) (B N)])
             ([(A T) (B T)] [(A N) (B T)] [(A N) (B N)]))

           '[(A M) (B M)]
           '(([(A M) (B T)] [(A M) (B N)] [(A T) (B N)] [(A N) (B N)])
             ([(A M) (B T)] [(A T) (B T)] [(A T) (B N)] [(A N) (B N)])
             ([(A M) (B T)] [(A T) (B T)] [(A N) (B T)] [(A N) (B N)])

             ([(A T) (B M)] [(A N) (B M)] [(A N) (B T)] [(A N) (B N)])
             ([(A T) (B M)] [(A T) (B T)] [(A T) (B N)] [(A N) (B N)])
             ([(A T) (B M)] [(A T) (B T)] [(A N) (B T)] [(A N) (B N)]))))))

(define/contract (add-paths paths point parent-points)
  ((hash/c point? (set/c path?))
   point?
   (set/c point?)
   . -> .
   (hash/c point? (set/c path?)))

  (define point-paths (hash-ref paths point))
  (for*/fold ([new-paths paths])
             ([parent (in-set parent-points)]
              [point-path (in-set point-paths)])
    (define parent-paths (hash-ref new-paths parent (const (set))))
    (define new-path (cons point point-path))
    (hash-set new-paths
              parent
              (set-add parent-paths new-path))))

(module+ test
  (define-test (test-add-paths paths point parent-points new-paths)
    (define config/parsed (parse-config point))
    (define parent-configs/parsed (list->set (map parse-config parent-points)))
    (define paths/parsed (parse-paths-hash paths))
    (define new-paths/parsed (parse-paths-hash new-paths))
    (define actual (add-paths paths/parsed config/parsed parent-configs/parsed))
    (test-path-maps=? actual new-paths/parsed))

  (test-begin
    (test-add-paths (hash '[(A N) (B N)] '(()))
                    '[(A N) (B N)]
                    '{[(A T) (B N)]
                      [(A N) (B T)]}

                    (hash '[(A N) (B N)] '(())
                          '[(A T) (B N)] '(([(A N) (B N)]))
                          '[(A N) (B T)] '(([(A N) (B N)]))))
    (test-add-paths
     (hash '[(A N) (B N)] '(())
           '[(A T) (B N)] '(([(A N) (B N)]))
           '[(A N) (B T)] '(([(A N) (B N)]))
           '[(A T) (B T)] '(([(A T) (B N)] [(A N) (B N)]))
           '[(A M) (B N)] '(([(A T) (B N)] [(A N) (B N)])))
     '[(A T) (B T)]
     '{[(A M) (B T)]
       [(A T) (B M)]}

     (hash '[(A N) (B N)] '(())
           '[(A T) (B N)] '(([(A N) (B N)]))
           '[(A N) (B T)] '(([(A N) (B N)]))
           '[(A T) (B T)] '(([(A T) (B N)] [(A N) (B N)]))
           '[(A M) (B N)] '(([(A T) (B N)] [(A N) (B N)]))
           '[(A M) (B T)] '(([(A T) (B T)] [(A T) (B N)] [(A N) (B N)]))
           '[(A T) (B M)] '(([(A T) (B T)] [(A T) (B N)] [(A N) (B N)]))))
    (test-add-paths
     (hash '[(A N) (B N)] '(())
           '[(A T) (B N)] '(([(A N) (B N)]))
           '[(A N) (B T)] '(([(A N) (B N)]))
           '[(A T) (B T)] '(([(A T) (B N)] [(A N) (B N)]))
           '[(A M) (B N)] '(([(A T) (B N)] [(A N) (B N)]))
           '[(A M) (B T)] '(([(A T) (B T)] [(A T) (B N)] [(A N) (B N)]))
           '[(A T) (B M)] '(([(A T) (B T)] [(A T) (B N)] [(A N) (B N)])))
     '[(A M) (B N)]
     '{[(A M) (B T)]}

     (hash '[(A N) (B N)] '(())
           '[(A T) (B N)] '(([(A N) (B N)]))
           '[(A N) (B T)] '(([(A N) (B N)]))
           '[(A T) (B T)] '(([(A T) (B N)] [(A N) (B N)]))
           '[(A M) (B N)] '(([(A T) (B N)] [(A N) (B N)]))
           '[(A M) (B T)] '(([(A M) (B N)] [(A T) (B N)] [(A N) (B N)])
                            ([(A T) (B T)] [(A T) (B N)] [(A N) (B N)]))
           '[(A T) (B M)] '(([(A T) (B T)] [(A T) (B N)] [(A N) (B N)]))))))

(module+ test
  (display-test-results))
