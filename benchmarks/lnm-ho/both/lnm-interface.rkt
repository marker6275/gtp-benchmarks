#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(require (only-in "summary-adapted.rkt" Summary))
(provide Summary)

(require/typed/check/provide "summary.rkt"
  [from-rktd (->* [String] [#:graph (U Path #f)] Summary)]
  [all-variations (-> Summary (Sequenceof String))]
  [get-num-variations (-> Summary Index)]
  [get-project-name (-> Summary String)]
  [predicate->variations (-> Summary (-> String Boolean) (Sequenceof String))]
  [untyped-mean (-> Summary Real)]
  [variation->mean-runtime (-> Summary String Real)])

(require/typed/check/provide "bitstring.rkt"
  [in-reach (-> String Index (Listof String))]
  [log2 (-> Index Index)]
  [natural->bitstring (-> Index #:pad Index String)])
