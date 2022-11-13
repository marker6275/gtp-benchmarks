#lang typed/racket/base

(require "../../../utilities/require-typed-check-provide.rkt")

(reprovide (except-in "structs-adapted.rkt"
                      Exp
                      Label
                      Var)
           (except-in "time-adapted.rkt"
                      Value)
           (except-in "denotable-adapted.rkt"
                      Denotable
                      Store)
           (except-in "benv-adapted.rkt"
                      BEnv
                      Addr
                      Time))

(provide Exp
         Label
         Var

         Value

         Denotable
         Store

         BEnv
         Addr
         Time)

(define-type Exp (U exp Ref Lam Call))
(define-type Label Symbol)
(define-type Var Symbol)

(define-type Value Closure)

(define-type Denotable (Setof Value))
(define-type Store (HashTable Addr Denotable))

(define-type BEnv (HashTable Var Addr))
(define-type Addr Binding)
(define-type Time (Listof Label))

(require/typed/check/provide "ai.rkt"
                             ;; Unused in client
  ;; (atom-eval (-> BEnv Store (-> Exp Denotable)))
  ;; (next (-> State (Setof State)))
  (explore (-> (Setof State) (Listof State) (Setof State)))
  )



