#lang racket/base

;; AKA quick-test.rkt

;; -----------------------------------------------------------------------------

(require
 (only-in typed/racket/class new send)
 "quad-interface.rkt"
)
(require (only-in "quad-main.rkt"
  typeset))

;; =============================================================================

(parameterize ([world:quality-default world:draft-quality])
  (time
    (begin
      (define to (typeset (quick-sample)))
      (send (new pdf-renderer%) render-to-file to "./output.pdf")
      (void))))
