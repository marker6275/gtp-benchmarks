#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(reprovide "image-adapted.rkt")

(require/typed/check/provide
 "zombie.rkt"
 (w0 World)
 (world-on-mouse (-> World (-> Real Real String World)))
 (world-on-tick (-> World (-> World))))

(provide World)
(define-type World
  (-> Symbol (U (Pairof 'on-mouse (-> Real Real String World))
                (Pairof 'on-tick (-> World))
                (Pairof 'to-draw (-> Image))
                (Pairof 'stop-when (-> Boolean)))))
