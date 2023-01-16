(module sequencer racket/base
  (#%module-begin
   (require "type-interface.rkt" (only-in "synth.rkt" fs) (only-in "mixer.rkt" mix))
   (provide sequence note)
   (define (note-freq note)
     (define res (* 440 (expt (expt 2 1/12) (- note 57))))
     (if (flonum? res) res (error "not real")))
   (define (name+octave->note name octave)
     (+
      (* 12 octave)
      (case name
        ((C) 0)
        ((C# Db) 1)
        ((D) 2)
        ((D# Eb) 3)
        ((E) 4)
        ((F) 5)
        ((F# Gb) 6)
        ((G) 7)
        ((G# Ab) 8)
        ((A) 9)
        ((A# Bb) 10)
        ((B) 11)
        (else 0))))
   (define (note name octave duration) (cons (name+octave->note name octave) duration))
   (define (synthesize-note note n-samples function)
     (build-array (vector n-samples) (if note (function (note-freq note)) (lambda (x) 0.0))))
   (define (sequence n pattern tempo function)
     (define samples-per-beat (quotient (* fs 60) tempo))
     (array-append*
      (for*/list
       ((i (in-range n)) (note (in-list pattern)))
       (define nsamples (* samples-per-beat (cdr note)))
       (synthesize-note (car note) nsamples function))))))
