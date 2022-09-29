#lang typed/racket

(struct: posn ([x : Real]
               [y : Real])
  #:prefab
  #:type-name Posn)
(struct: block ([x : Real]
                [y : Real]
                [color : Symbol])
  #:prefab
  #:type-name Block)
(struct: tetra ([center : Posn]
                [blocks : (Listof Block)])
  #:prefab
  #:type-name Tetra)
(struct: world ([tetra : Tetra]
                [blocks : (Listof Block)])
  #:prefab
  #:type-name World)

(: posn=? (-> Posn Posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(provide
 ;; (struct-out posn)
 ;; (struct-out block)
 ;; (struct-out tetra)
 ;; (struct-out world)
 posn=?)
