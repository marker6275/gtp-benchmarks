#lang typed/racket

(require "data-adaptor.rkt")

(struct: snake ([dir  : Dir]
                [segs : NEListof-Posn])
  #:prefab
  #:type-name Snake)
(struct: world ([snake : Snake]
                [food  : Posn])
  #:prefab
  #:type-name World)

(struct: posn ([x : Real]
               [y : Real])
  #:prefab
  #:type-name Posn)

;; NeSegs is one of:
;; - (cons Posn empty)
;; - (cons Posn NeSegs)

;; cut-tail : NeSegs -> Segs
;; Cut off the tail.
(: cut-tail : (NEListof-Posn . -> . (Listof Posn)))
(define (cut-tail segs)
  (let ([r (cdr segs)])
    (cond [(empty? r) empty]
          [else (cons (car segs) (cut-tail r))])))

(provide
 cut-tail)
