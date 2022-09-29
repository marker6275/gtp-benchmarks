#lang typed/racket/base

(require "basics-types.rkt")
(struct card (
 [face : Face]
 [bulls : Bulls])
  #:prefab
  #:type-name Card)

(provide Stack)
(define-type Stack
  (Listof Card))
