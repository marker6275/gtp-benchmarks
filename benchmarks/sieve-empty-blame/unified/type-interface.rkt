(module mutation-adapter typed/racket/shallow
  (#%module-begin
   (module contracted racket
     (require racket/require
              (path-up "blame-evaluation-gt/mutation-adapter/mutation-adapter.rkt"))
     (require "original-type-interface.rkt")
     (provide (except-out (all-from-out "original-type-interface.rkt") stream-unfold))
     (provide (contract-out (stream-unfold (swap-> #f 0 1)))))
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (require/typed/check/provide
    'contracted
    (#:struct stream ((first : Natural) (rest : (-> stream))))
    (make-stream (-> Natural (-> stream) stream))
    (stream-unfold (-> stream (values Natural stream)))
    (stream-get (-> stream Natural Natural))
    (stream-take (-> stream Natural (Listof Natural))))))
