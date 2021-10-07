#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(require/typed/check/provide "streams.rkt"
  [#:struct stream ([first : Natural]
                    [rest : (-> stream)])]
  [make-stream (-> Natural (-> stream) stream)]
  [stream-unfold (-> stream (values Natural stream))]
  [stream-get (-> stream Natural Natural)]
  [stream-take (-> stream Natural (Listof Natural))])
