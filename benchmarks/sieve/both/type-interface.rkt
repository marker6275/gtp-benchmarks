#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(struct: stream ([first : Natural] [rest : (-> stream)])
  #:prefab)

(provide (struct-out stream))

(require/typed/check/provide "streams.rkt"
  [make-stream (-> Natural (-> stream) stream)]
  [stream-unfold (-> stream (values Natural stream))]
  [stream-get (-> stream Natural Natural)]
  [stream-take (-> stream Natural (Listof Natural))])
