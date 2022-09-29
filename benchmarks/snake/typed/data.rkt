#lang typed/racket

(struct: snake ([dir  : Dir]
                [segs : (NEListof Posn)])
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

(define-type (NEListof A) (Pairof A (Listof A)))
(define-type Dir (U "up" "down" "left" "right"))

(: posn=? (-> Posn Posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))  

(provide
 posn=?
 ;; [struct-out posn]
 ;; [struct-out snake]
 ;; [struct-out world]
 )
