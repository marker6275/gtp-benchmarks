#lang racket/base

(provide command%
         binop-command%)

(require "../base/untyped.rkt"
         racket/match
         racket/class
         (only-in racket/string string-join)
         (for-syntax racket/base racket/syntax syntax/parse))

(require (only-in "stack.rkt"
  stack-drop
  stack-dup
  stack-init
  stack-over
  stack-pop
  stack-push
  stack-swap
))

(define command%
  (class object%
    (super-new)
    (init-field
      id
      descr
      exec)))

;; True if the argument is a list with one element
(define (singleton-list? x)
  (and (pair? x)
       (null? (cdr x))))

;; Create a binary operation command.
;; Command is recognized by its identifier,
;;  the identifier is then applied to the top 2 numbers on the stack.
(define binop-command%
  (class command%
    (init-field
     binop)
    (super-new
      (id (assert (object-name binop) symbol?))
      (exec (lambda (E S v)
        (if (singleton-list? v)
          (if (eq? (car v) (get-field id this))
             (let*-values ([(v1 S1) (stack-pop S)]
                           [(v2 S2) (stack-pop S1)])
               (cons E (stack-push S2 (binop v2 v1))))
             #f)
           #f))))))
