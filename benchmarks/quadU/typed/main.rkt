#lang typed/racket/base

;; -----------------------------------------------------------------------------

(require
 require-typed-check
 (only-in typed/racket/class new send)
 "quad-interface.rkt")

(require/typed/check "quad-main.rkt"
  (typeset (-> Quad Quad))
)

;; =============================================================================

(parameterize ([world:quality-default world:draft-quality])
  (time
    (begin
      (define to (typeset (quick-sample)))
      (send (new pdf-renderer%) render-to-file to "./output.pdf")
      (void))))
