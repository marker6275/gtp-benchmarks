#lang racket/base

(provide for-player)

(require racket/class
         racket/list)

(require (only-in "stack.rkt"
                  bulls))

;; Class[my-stacks field] -> Class[my-stacks field and fewest-bulls method]
(define (for-player deck%)
  (class deck%
    (inherit-field my-stacks)
    (super-new)

    (define/public (fewest-bulls)
      (define stacks-with-bulls
        (for/list
                  ((s my-stacks))
          (list s (bulls s))))
      (first (argmin (lambda (l) (second l)) stacks-with-bulls)))))
