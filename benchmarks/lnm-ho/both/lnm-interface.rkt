#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(reprovide "summary-adapted.rkt")
(require/typed/check/provide "bitstring.rkt"
  [in-reach (-> String Index (Listof String))]
  [log2 (-> Index Index)]
  [natural->bitstring (-> Index #:pad Index String)])
