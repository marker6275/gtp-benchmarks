#lang typed/racket

(require require-typed-check
         "../base/command-types.rkt")

(provide command%
         binop-command%)

(require/typed/check "stack.rkt"
  (stack-drop (-> Stack Stack))
  (stack-dup (-> Stack Stack))
  (stack-init (-> Stack))
  (stack-over (-> Stack Stack))
  (stack-pop (-> Stack (Values Integer Stack)))
  (stack-push (-> Stack Integer Stack))
  (stack-swap (-> Stack Stack))
)

(: command% Command%)
(define command%
  (class object%
    (super-new)
    (init-field
      id
      descr
      exec)))

;; True if the argument is a list with one element
(: singleton-list? (-> Any Boolean : (List Any)))
(define (singleton-list? x)
  (and (pair? x)
       (null? (cdr x))))

;; Create a binary operation command.
;; Command is recognized by its identifier,
;;  the identifier is then applied to the top 2 numbers on the stack.
(: binop-command% Binop-Command%)
(define binop-command%
  (class command%
    (init-field
     binop)
    (super-new
      (id (assert (object-name binop) symbol?))
      (exec (lambda ([E : Env] [S : Stack] [v : Any])
        (if (singleton-list? v)
          (if (eq? (car v) (get-field id this))
             (let*-values ([(v1 S1) (stack-pop S)]
                           [(v2 S2) (stack-pop S1)])
               (cons E (stack-push S2 (binop v2 v1))))
             #f)
           #f))))))
