#lang typed/racket/base

(require "../../../utilities/require-typed-check-provide.rkt")

;; (reprovide "data-adapter.rkt")

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

(define-type Color Symbol)
(define-type BSet  (Listof Block))
(provide (struct-out posn)
         Posn
         (struct-out block)
         Block
         (struct-out tetra)
         Tetra
         (struct-out world)
         World
         Color
         BSet)

(require/typed/check/provide "bset.rkt"
   [blocks-overflow? (-> BSet Boolean)]
   [blocks-union (-> BSet BSet BSet)]
   [blocks-max-x (-> BSet Real)]
   [blocks-min-x (-> BSet Real)]
   [blocks-max-y (-> BSet Real)])
(require/typed/check/provide "tetras.rkt"
  [tetra-move (-> Real Real Tetra Tetra)]
  [tetra-rotate-ccw (-> Tetra Tetra)]
  [tetra-rotate-cw (-> Tetra Tetra)]
  [tetra-overlaps-blocks? (-> Tetra BSet Boolean)]
  [tetra-change-color (-> Tetra Color Tetra)])
(require/typed/check/provide "aux.rkt"
  [list-pick-random (-> (Listof Tetra) Tetra)]
  [neg-1  Negative-Fixnum]
  [tetras (Listof Tetra)])
(require/typed/check/provide "elim.rkt"
  [eliminate-full-rows (-> BSet BSet)])
(require/typed/check/provide "consts.rkt"
  [board-height Integer]
  [board-width Integer])
