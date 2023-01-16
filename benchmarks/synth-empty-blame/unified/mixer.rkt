(module mixer racket/base
  (#%module-begin
   (require "type-interface.rkt" (only-in racket/list first second rest))
   (provide mix)
   (require (for-syntax racket/base))
   (define-syntax-rule
    (ensure-array name arr-expr)
    (let ((arr arr-expr)) (if (array? arr) arr (raise-argument-error name "Array" arr))))
   (define-syntax (inline-array-map stx)
     (syntax-case stx ()
       ((_ f arr-expr)
        (syntax/loc
         stx
         (let ((arr (ensure-array 'array-map arr-expr)))
           (define ds (array-shape arr))
           (define proc (unsafe-array-proc arr))
           (define arr* (unsafe-build-array ds (λ (js) (f (proc js)))))
           (array-default-strict! arr*)
           arr*)))
       ((_ f arr-expr arr-exprs ...)
        (with-syntax
         (((arrs ...) (generate-temporaries #'(arr-exprs ...)))
          ((procs ...) (generate-temporaries #'(arr-exprs ...))))
         (syntax/loc
          stx
          (let ((arr (ensure-array 'array-map arr-expr))
                (arrs (ensure-array 'array-map arr-exprs))
                ...)
            (define ds (array-shape-broadcast (list (array-shape arr) (array-shape arrs) ...)))
            (let ((arr (array-broadcast arr ds)) (arrs (array-broadcast arrs ds)) ...)
              (define proc (unsafe-array-proc arr))
              (define procs (unsafe-array-proc arrs))
              ...
              (define arr* (unsafe-build-array ds (λ (js) (f (proc js) (procs js) ...))))
              (array-default-strict! arr*)
              arr*)))))))
   (define array-map
     (case-lambda
      ((f arr) (inline-array-map f arr))
      ((f arr0 arr1) (inline-array-map f arr0 arr1))))
   (define (mix . ss)
     (define signals (for/list ((s ss)) (first s)))
     (define weights (for/list ((x ss)) (real->double-flonum (second x))))
     (define downscale-ratio (/ 1.0 (apply + weights)))
     (define ((scale-signal w) x) (* x w downscale-ratio))
     (parameterize
      ((array-broadcasting 'permissive))
      (for/fold
       ((res (array-map (scale-signal (first weights)) (first signals))))
       ((s (in-list (rest signals))) (w (in-list (rest weights))))
       (define scale (scale-signal w))
       (array-map (lambda (acc new) (+ acc (scale new))) res s))))))
