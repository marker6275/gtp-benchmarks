#lang racket

(provide (contract-out [precision-config-lattice
                        ((listof path-string?)
                         (listof symbol?)
                         . -> .
                         lattice?)])
         (struct-out mod-config)
         (struct-out lattice-point))

(require "lattice.rkt")

;; The precision config lattice is composed of a bunch of
;; `bench-config`s, where each `bench-config` is a set of
;; `mod-config`s: it represents a configuration with a setting of
;; precision level per module.

;; mod-config = path-string? symbol?
(struct mod-config (mod level) #:transparent)

(define bench-config? (set/c mod-config?))

(define (precision-config-lattice modules levels)
  (define base-level (first levels))
  (define initial-config
    (for/set ([mod modules])
      (mod-config mod base-level)))
  (make-lattice initial-config (parents-of levels)))

(define/match (increment-mod-config mc levels)
  [{(mod-config m level) levels}
   (define index (index-of levels level))
   (mod-config m (list-ref levels (add1 index)))])

(define/match (mod-config-max? mc levels)
  [{(mod-config _ level) levels}
   (equal? level (last levels))])

(define/contract ((parents-of levels) bench-config)
  ((listof symbol?) . -> . (bench-config? . -> . (set/c bench-config?)))
  (for/fold ([parents (set)])
            ([mod-config bench-config])
    (define other-mods (set-remove bench-config mod-config))
    (cond [(mod-config-max? mod-config levels) parents]
          [else
           (define bench-config-with-mod+1
             (set-add other-mods (increment-mod-config mod-config levels)))
           (set-add parents bench-config-with-mod+1)])))

(module+ test
  (require ruinit)

  (define (parse-config config-as-list)
    (for/set ([mod-as-list config-as-list])
      (mod-config (first mod-as-list) (second mod-as-list))))
  (define (parse-paths-hash p)
    (for/hash ([(c path-list) p])
      (values (parse-config c)
              (list->set (map (curry map parse-config) path-list)))))
  (define-test (test-parents-of config parent-configs)
    (define config/parsed (parse-config config))
    (define parent-configs/parsed (list->set (map parse-config parent-configs)))
    (test-equal? ((parents-of '(N T M)) config/parsed)
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
                       [(A T) (B T) (C T)]})

    (ignore
     (define h
       (parse-paths-hash
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
    (test-match (precision-config-lattice '(A B) '(N T M))
                (lattice
                 (lattice-point (== (parse-config '[(A N) (B N)])) '())
                 (lattice-point (== (parse-config '[(A M) (B M)])) _)
                 (== (for/set ([(point paths) h])
                       (lattice-point point paths))))))

  (display-test-results))
