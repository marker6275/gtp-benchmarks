#lang typed/racket

(define-type Color Symbol)
(struct: posn ([x : Real]
               [y : Real])
  #:prefab
  #:type-name Posn)
(struct: block ([x : Real]
                [y : Real]
                [color : Symbol])
  #:prefab
  #:type-name Block)

(define-type BSet  (Listof Block))

(provide
 Color
 BSet)
