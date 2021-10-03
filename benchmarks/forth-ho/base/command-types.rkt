#lang typed/racket/base

(provide
  Stack
  Env
  Val
  State
  Result
  Command%
  Binop-Command%)

(require typed/racket/class)

(define-type Stack (Listof Integer)) ;;bg; not good
(define-type Env (Listof (Instance Command%)))
(define-type Val (U Symbol Integer))
(define-type State Stack)
(define-type Result (U 'EXIT #f (Pairof Env State)))

(define-type Command%
  (Class
    (init-field
      [id Symbol]
      [descr String]
      [exec (-> Env State Any Result)])))

(define-type Binop-Command%
  (Class
   (init
     (binop (-> Integer Integer Integer))
     (descr String))
   (field
     (binop (-> Integer Integer Integer))
     (descr String)
     (exec (-> Env State Any (U False 'EXIT (Pairof Env State))))
     (id Symbol))))
