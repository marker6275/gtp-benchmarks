#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt"
         "../base/command-types.rkt")

(provide (all-from-out "../base/command-types.rkt"))

(require/typed/check/provide
 "stack.rkt"
 [stack-drop (-> Stack Stack)]
 [stack-dup (-> Stack Stack)]
 [stack-init (-> Stack)]
 [stack-over (-> Stack Stack)]
 [stack-pop (-> Stack (Values Integer Stack))]
 [stack-push (-> Stack Integer Stack)]
 [stack-swap (-> Stack Stack)])

(require/typed/check/provide
 "command-base.rkt"
 [command% Command%]
 [binop-command% Binop-Command%])

(require/typed/check/provide
 "eval.rkt"
 [forth-eval* (-> (Listof String) (Listof (Instance Command%)) (Values (Option Env) Stack))])
