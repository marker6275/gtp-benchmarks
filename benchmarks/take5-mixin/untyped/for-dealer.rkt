#lang racket/base

(provide for-dealer
         base-deck%)

(require racket/class
         racket/list
         "card.rkt")

(require (only-in "basics.rkt"
                  FACE))

(require (only-in "stack.rkt"
                  bulls))

;; Class[cards0 field] -> Class[fit, push, replace & larger-than-some-top-of-stacks? methods]
(define (for-dealer deck%)
  (class deck%
    (inherit-field cards0)
    (inherit-field my-stacks)
    (super-new)

    ;; [Listof Stack]
    (set-field! my-stacks this (map (lambda (c) (list c)) cards0))
    ;(field [my-stacks

    (define/public (fit c)
      (define (distance stack)
        (define d (first stack))
        (if (>-face c d) (--face c d) (+ FACE 1)))
      (argmin distance my-stacks))

    (define/public (push c)
      (define s0 (fit c))
      (void (replace-stack (first s0) c)))

    (define/public (replace s c)
      (replace-stack (first s) (list c)))

    (define/public (replace-stack top0 c)
      (define result  0)
      (set! my-stacks 
            (for/list  ((s  my-stacks))
              (cond
                [(equal? (first s) top0)
                 (set! result (bulls s))
                 (if (cons? c)
                  c
                  (if (null? c) (error 'invalid-input) (cons c s)))]
                [else s])))
      result)

    (define/public (larger-than-some-top-of-stacks? c)
      (for/or ((s my-stacks))
        (>-face c (first s))))))

;; Class[cards0 field]
(define base-deck%
  (class object%
    (init-field
     ;; [Listof Card]
     ;; the tops of the initial stacks (for a round)
     cards0)

    (field (my-stacks '()))

    (super-new)))
