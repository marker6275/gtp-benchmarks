#lang racket

(module+ test
  (require ruinit))

(define (precision-config-lattice modules levels)
  (define base-level (first levels))
  (define initial-config
    (for/set ([mod modules])
      (mod-config mod base-level)))
  (make-lattice initial-config
                (match-lambda
                  [(mod-config m level)
                   (define index (index-of levels level))
                   (mod-config m (list-ref levels (add1 index)))])
                (match-lambda
                  [(mod-config _ level)
                   (equal? level (last levels))])))


;; ModuleConfig = Module x Level
(struct mod-config (mod level) #:transparent)

(define bench-config? (set/c mod-config?))

;; point is `bench-config?` and a set of points (parents)
(struct point (config parents))

(define path? (listof bench-config?))


(module+ test
  (define inc (match-lambda [(mod-config m 'T) (mod-config m 'M)]
                            [(mod-config m 'N) (mod-config m 'T)]))
  (define max? (match-lambda [(mod-config m 'M) #t]
                             [_ #f]))

  (define (parse-config config-as-list)
    (for/set ([mod-as-list config-as-list])
      (mod-config (first mod-as-list) (second mod-as-list))))
  (define (parse-paths-hash p)
      (for/hash ([(c path-list) p])
        (values (parse-config c)
                (map (curry map parse-config) path-list))))
  (define-test (test-path-maps=? actual expected)
    (unless (equal? actual expected)
      (fail "Path maps differ.\nActual:\n~a\nExpected:\n~a\n"
            (pretty-format actual)
            (pretty-format expected)))))

(define/contract (parents-of bench-config increment max?)
  (bench-config?
   (mod-config? . -> . mod-config?)
   (mod-config? . -> . boolean?)
   . -> .
   (set/c bench-config?))

  (for/fold ([parents (set)])
            ([mod-config bench-config])
    (define other-mods (set-remove bench-config mod-config))
    (cond [(max? mod-config) parents]
          [else
           (define bench-config-with-mod+1
             (set-add other-mods (increment mod-config)))
           (set-add parents bench-config-with-mod+1)])))

(module+ test
  (define-test (test-parents-of config parent-configs)
    (define config/parsed (parse-config config))
    (define parent-configs/parsed (list->set (map parse-config parent-configs)))
    (test-equal? (parents-of config/parsed inc max?)
                 parent-configs/parsed))

  (test-begin
    (test-parents-of '[(A N) (B N)]
                     '{[(A T) (B N)]
                       [(A N) (B T)]})
    (test-parents-of '[(A T) (B T)]
                     '{[(A M) (B T)]
                       [(A T) (B M)]})
    (test-parents-of '[(A T) (B T) (C N)]
                     '{[(A M) (B T) (C N)]
                       [(A T) (B M) (C N)]
                       [(A T) (B T) (C T)]})))

(define/contract (make-lattice start increment max?)
  (bench-config?
   (mod-config? . -> . mod-config?)
   (mod-config? . -> . boolean?)
   . -> .
   (hash/c bench-config? (listof path?)))

  (let loop (;; hash of bench-config? -> (listof path?)
             [paths (hash start '(()))]
             ;; set of bench-config?
             [seen (set)]
             ;; set of bench-config?
             [to-process (set start)])
    (cond [(set-empty? to-process)
           paths]

          [else
           (define bench (set-first to-process))
           (define parents (parents-of bench increment max?))
           (define parents/unseen (set-subtract parents seen))
           (define paths+bench->parents
             (add-paths paths bench parents))
           (loop paths+bench->parents
                 (set-add seen bench)
                 (set-union (set-rest to-process)
                            parents/unseen))])))

(module+ test
  (define-test (test-lattice start expected)
    (define start/parsed (parse-config start))
    (define expected/parsed (parse-paths-hash expected))
    (test-path-maps=? (make-lattice start/parsed inc max?)
                      expected/parsed))
  (test-begin
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

(define/contract (add-paths paths config parent-configs)
  ((hash/c bench-config? (listof path?))
   bench-config?
   (set/c bench-config?)
   . -> .
   (hash/c bench-config? (listof path?)))

  (define config-paths (hash-ref paths config))
  (for*/fold ([new-paths paths])
             ([parent parent-configs]
              [config-path config-paths])
    (define parent-paths (hash-ref new-paths parent (const empty)))
    (define new-path (cons config config-path))
    (hash-set new-paths
              parent
              (cons new-path parent-paths))))

(module+ test
  (define-test (test-add-paths paths config parent-configs new-paths)
    (define config/parsed (parse-config config))
    (define parent-configs/parsed (list->set (map parse-config parent-configs)))
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
