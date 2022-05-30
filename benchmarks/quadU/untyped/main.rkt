#lang racket/base

;; -----------------------------------------------------------------------------

(require
 require-typed-check
 (only-in typed/racket/class new send)
 "type-interface.rkt")

(require (only-in "quad-main.rkt"
  typeset; (-> Quad Quad))
))

;; =============================================================================

(parameterize ([world:quality-default world:draft-quality])
  (time
    (begin
      (define to (typeset (quick-sample)))
      (send (new pdf-renderer%) render-to-file to "./output.pdf")
      (void))))
