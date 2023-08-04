#lang racket


(require "data.rkt"
         "const.rkt"
         "handlers.rkt"
         "motion.rkt"
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

;; (provide/configurable-contract
;;  [replay ([max (world-type?
;;                 (listof (or/c (list/c 'on-key string?)
;;                               (list/c 'on-tick)
;;                               (list/c 'stop-when)))
;;                 . -> .
;;                 void?)]
;;           [types (world-type? (listof (listof (or/c symbol? string?))) . -> . void?)])]
;;  [DATA any/c]
;;  [LOOPS ([max (=/c 1)]
;;          [types natural?])]
;;  [main ([max ((listof (or/c (list/c 'on-key string?)
;;                             (list/c 'on-tick)
;;                             (list/c 'stop-when)))
;;               . -> .
;;               void?)]
;;         [types ((listof (listof (or/c symbol? string?))) . -> . void?)])])

(define (replay w0 hist)
  (reset!)
  (for/fold ([w w0])
            ([cmd (in-list hist)])
    (match cmd
      [`(on-key ,(? string? ke))
       (handle-key w ke)]
      [`(on-tick)
       (world->world w)]
      [`(stop-when)
       (game-over? w)
       w]))
  (void))

(define DATA
  (with-input-from-file "../base/snake-hist.rktd" read))
(define LOOPS
  1)

(define (main hist)
  (define w0 (WORLD))
  (cond [(list? hist)
         (for ((_i (in-range LOOPS)))
           (replay w0 hist))]
        [else
         (error "bad input")]))

(time (main DATA))
