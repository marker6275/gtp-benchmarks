(module type-interface typed/racket/shallow
  (#%module-begin
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (require/typed/check/provide
    "streams.rkt"
    (#:struct stream ((first : Natural) (rest : (-> stream))))
    (make-stream (-> Natural (-> stream) stream))
    (stream-unfold (-> stream (values stream Natural)))
    (stream-get (-> stream Natural Natural))
    (stream-take (-> stream Natural (Listof Natural))))))
