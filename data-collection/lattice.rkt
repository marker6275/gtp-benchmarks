#lang racket

(provide (contract-out
          [make-lattice (point?
                         (point? . -> . (set/c point?))
                         . -> .
                         lattice?)])
         (struct-out lattice-point)
         (struct-out lattice))


(define point? any/c)

(define path? (listof point?))

;; value:    point?
;; paths-to: (set/c path?)
(struct lattice-point (value paths-to) #:transparent)

;; min:    lattice-point?
;; max:    lattice-point?
;; points: (set/c lattice-point?)
(struct lattice (min max points) #:transparent)

;; point? (point? . -> . (set/c point?)) -> lattice?
(define (make-lattice start parents-of)
  (define-values (points-hash max) (explore-lattice start parents-of))
  (define min-point (lattice-point start '()))
  (define max-point (lattice-point max (hash-ref points-hash max)))
  (define points (for/set ([(point paths) points-hash])
                   (lattice-point point paths)))
  (lattice min-point max-point points))





(module+ test
  (require ruinit)
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
                (list->set (map (curry map parse-config) path-list))))))


(define/contract (explore-lattice start parents-of)
  (point?
   (point? . -> . (set/c point?))
   . -> .
   (values (hash/c point? (set/c path?))
           point?))

  (let loop (;; hash of point? -> (set/c path?)
             [paths (hash start (set '()))]
             ;; set of point?
             [seen (set)]
             ;; list of point?
             [to-process (list start)]
             ;; point?
             [last-processed start])
    (cond [(empty? to-process)
           (values paths last-processed)]

          [else
           (define point (first to-process))
           (define parents (parents-of point))
           (define parents/unseen
             (set-subtract (set->list (set-subtract parents seen))
                           to-process))
           (define paths+bench->parents (add-paths paths point parents))
           (loop paths+bench->parents
                 (set-add seen point)
                 (append (rest to-process) parents/unseen)
                 point)])))

(module+ test
  (define-test (test-lattice start expected)
    (define start/parsed (parse-config start))
    (define expected/parsed (parse-paths-hash expected))
    (define-values (lattice-hash max) (explore-lattice start/parsed parents-of))
    (test-equal? lattice-hash expected/parsed))
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
    (test-equal? actual new-paths/parsed))

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
