(module synth racket/base
  (#%module-begin
   (provide fs sawtooth-wave seconds->samples emit)
   (require "type-interface.rkt"
            (only-in racket/unsafe/ops unsafe-fx+ unsafe-fx<)
            (only-in racket/math exact-floor)
            "../base/untyped.rkt")
   (require (for-syntax racket/base syntax/parse))
   (define-sequence-syntax
    in-array
    (λ () #'in-array)
    (λ (stx)
      (syntax-case stx ()
        (((x) (_ arr-expr))
         (syntax/loc
          stx
          ((x)
           (:do-in
            (((ds size dims js proc)
              (let ((arr arr-expr))
                (cond
                 ((array? arr)
                  (define ds (array-shape arr))
                  (define dims (vector-length ds))
                  (define size (array-size arr))
                  (define proc (unsafe-array-proc arr))
                  (define js (make-vector dims 0))
                  (values ds size dims js proc))
                 (else (raise-argument-error 'in-array "Array" arr))))))
            (void)
            ((j 0))
            (unsafe-fx< j size)
            (((x) (proc js)))
            #t
            #t
            ((begin (next-indexes! ds dims js) (unsafe-fx+ j 1)))))))
        ((_ clause)
         (raise-syntax-error 'in-array "expected (in-array <Array>)" #'clause #'clause)))))
   (array-strictness #f)
   (define fs 44100)
   (define bits-per-sample 16)
   (define (freq->sample-period freq)
     (define res (inexact->exact (round (/ fs freq))))
     (if (index? res) res (error "not index")))
   (define (seconds->samples s)
     (define res (inexact->exact (round (* s fs))))
     (if (index? res) res (error "not index")))
   (define-syntax-rule
    (array-lambda (i) body ...)
    (lambda (i*) (let ((i (vector-ref i* 0))) body ...)))
   (define ((make-sawtooth-wave coeff) freq)
     (define sample-period (freq->sample-period freq))
     (define sample-period/2 (quotient sample-period 2))
     (array-lambda
      (x)
      (define x* (exact->inexact (modulo x sample-period)))
      (* coeff (- (/ x* sample-period/2) 1.0))))
   (define sawtooth-wave (make-sawtooth-wave 1.0))
   (define (signal->integer-sequence signal #:gain (gain 1))
     (for/vector
      #:length
      (array-size signal)
      ((sample (in-array signal)))
      (max
       0
       (min
        (sub1 (expt 2 bits-per-sample))
        (exact-floor (* gain (* (+ sample 1.0) (expt 2 (sub1 bits-per-sample)))))))))
   (define (emit signal) (signal->integer-sequence signal #:gain 0.3))))
