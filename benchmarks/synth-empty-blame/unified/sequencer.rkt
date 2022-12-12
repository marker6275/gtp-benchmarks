(module sequencer typed/racket/shallow
  (#%module-begin
   (require "../../../utilities/require-typed-check-provide-transient.rkt" "type-interface.rkt")
   (require/typed/check "synth.rkt" (fs Natural))
   (require/typed/check "mixer.rkt" (mix (-> Weighted-Signal * Array)))
   (provide sequence note)
   (: note-freq (-> Natural Float))
   (define (note-freq note) (: res Real) (define res (* 440 (expt (expt 2 1/12) (- note 57)))) (if (flonum? res) res (error "not real")))
   (: name+octave->note (-> Symbol Natural Natural))
   (define (name+octave->note name octave)
     (+ (* 12 octave) (case name ((C) 0) ((C# Db) 1) ((D) 2) ((D# Eb) 3) ((E) 4) ((F) 5) ((F# Gb) 6) ((G) 7) ((G# Ab) 8) ((A) 9) ((A# Bb) 10) ((B) 11) (else 0))))
   (: note (-> Symbol Natural Natural (Pairof Natural Natural)))
   (define (note name octave duration) (cons (name+octave->note name octave) duration))
   (: synthesize-note (-> (U #f Natural) Natural (-> Float (-> Indexes Float)) Array))
   (define (synthesize-note note n-samples function) (build-array (vector n-samples) (if note (function (note-freq note)) (lambda (x) 0.0))))
   (: sequence (-> Natural (Listof (Pairof (U Natural #f) Natural)) Natural (-> Float (-> Indexes Float)) Array))
   (define (sequence n pattern tempo function)
     (: samples-per-beat Natural)
     (define samples-per-beat (quotient (* fs 60) tempo))
     (array-append*
      (for*/list
       :
       (Listof Array)
       ((i (in-range n)) (note : (Pairof (U Natural #f) Natural) (in-list pattern)))
       (: nsamples Natural)
       (define nsamples (* samples-per-beat (cdr note)))
       (synthesize-note (car note) nsamples function))))))
