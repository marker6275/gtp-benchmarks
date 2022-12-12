(module synth typed/racket/shallow
  (#%module-begin
   (provide fs sawtooth-wave seconds->samples emit)
   (require "../../../utilities/require-typed-check-provide-transient.rkt" (only-in racket/unsafe/ops unsafe-fx+ unsafe-fx<) (only-in racket/math exact-floor) "type-interface.rkt")
   (require "../../../utilities/require-typed-check-provide-transient.rkt" (for-syntax racket/base syntax/parse))
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
              (let ((arr : Array arr-expr))
                (cond
                 ((array? arr)
                  (define ds (array-shape arr))
                  (define dims (vector-length ds))
                  (define size (array-size arr))
                  (define proc (unsafe-array-proc arr))
                  (define js : Indexes (make-vector dims 0))
                  (values ds size dims js proc))
                 (else (raise-argument-error 'in-array "Array" arr))))))
            (void)
            ((j 0))
            (unsafe-fx< j size)
            (((x) (proc js)))
            #t
            #t
            ((begin (next-indexes! ds dims js) (unsafe-fx+ j 1)))))))
        ((_ clause) (raise-syntax-error 'in-array "expected (in-array <Array>)" #'clause #'clause)))))
   (array-strictness #f)
   (: fs Natural)
   (define fs 44100)
   (: bits-per-sample Natural)
   (define bits-per-sample 16)
   (: freq->sample-period (-> Float Integer))
   (define (freq->sample-period freq) (: res Exact-Rational) (define res (inexact->exact (round (/ fs freq)))) (if (index? res) res (error "not index")))
   (: seconds->samples (-> Float Integer))
   (define (seconds->samples s) (: res Exact-Rational) (define res (inexact->exact (round (* s fs)))) (if (index? res) res (error "not index")))
   (define-syntax-rule (array-lambda (i) body ...) (lambda ((i* : (Vectorof Integer))) (let ((i : Integer (vector-ref i* 0))) body ...)))
   (: make-sawtooth-wave (-> Float (-> Float (-> Indexes Float))))
   (define ((make-sawtooth-wave coeff) freq)
     (: sample-period Integer)
     (define sample-period (freq->sample-period freq))
     (: sample-period/2 Integer)
     (define sample-period/2 (quotient sample-period 2))
     (array-lambda (x) (: x* Float) (define x* (exact->inexact (modulo x sample-period))) (* coeff (- (/ x* sample-period/2) 1.0))))
   (: sawtooth-wave (-> Float (-> Indexes Float)))
   (define sawtooth-wave (make-sawtooth-wave 1.0))
   (: signal->integer-sequence (-> Array (#:gain Float) (Vectorof Integer)))
   (define (signal->integer-sequence signal #:gain (gain 1))
     (for/vector
      :
      (Vectorof Integer)
      #:length
      (array-size signal)
      ((sample : Float (in-array signal)))
      (max 0 (min (sub1 (expt 2 bits-per-sample)) (exact-floor (* gain (* (+ sample 1.0) (expt 2 (sub1 bits-per-sample)))))))))
   (: emit (-> Array (Vectorof Integer)))
   (define (emit signal) (signal->integer-sequence signal #:gain 0.3))))
