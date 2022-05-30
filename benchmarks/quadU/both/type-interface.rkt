#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(reprovide "../base/quad-types.rkt")

(require/typed/check/provide "quick-sample.rkt"
  (quick-sample (-> Quad))
)
(require/typed/check/provide "render.rkt"
  (pdf-renderer%
    (Class
      [render-to-file (Quad Path-String -> Void)]
      [render-element (Quad -> Any)]
      [render-page ((Listof Quad) -> Void)]
      [render-word (Quad -> Any)]
      [render (-> Quad Any)]
      [finalize (-> Any Any)]
      [setup (-> Quad Quad)]))
)
(require/typed/check/provide "quads.rkt"
  (make-quadattrs (-> (Listof Any) QuadAttrs))
  (quad-car (-> Quad (U String Quad)))
  (line (->* ((Listof Any)) #:rest USQ Quad))
  (quads->column (-> (Listof Quad) Quad))
  (quads->page (-> (Listof Quad) Quad))
  (quads->block (-> (Listof Quad) Quad))
  (quad-has-attr? (-> Quad Symbol Boolean))
  (quad-name (-> Quad Symbol))
  (quad-attr-ref (((U Quad QuadAttrs) Symbol) (Any) . ->* . Any))
  (quad-list (-> Quad (Listof USQ)))
  (quad-attrs (-> Quad (Listof Any)))
  (quads->doc (-> (Listof Quad) Quad))
  (page (->* ((Listof Any)) #:rest USQ Quad))
  (column (->* ((Listof Any)) #:rest USQ Quad))
)
(require/typed/check/provide "wrap.rkt"
  (insert-spacers-in-line ((Quad) ((Option Symbol)) . ->* . Quad))
  ;(wrap-adaptive (->* ((Listof Quad)) (Float) (Listof Quad)))
  (wrap-best (->* ((Listof Quad)) (Float) (Listof Quad)))
  ;(wrap-first (->* ((Listof Quad)) (Float) (Listof Quad)))
  (fill (->* (Quad) ((Option Float)) Quad))
  (add-horiz-positions (-> Quad Quad))
)
(require/typed/check/provide "world.rkt"
  [world:quality-default (Parameterof Integer)]
  [world:line-looseness-key Symbol]
  [world:allow-hyphenated-last-word-in-paragraph Boolean]
  [world:line-looseness-tolerance Float]
  [world:line-index-key Symbol]
  [world:measure-key Symbol]
  [world:use-hyphenation? Boolean]
  [world:max-quality Index]
  [world:total-lines-key Symbol]
  [world:draft-quality Index]
  [world:quality-key Symbol]
  [world:quality-key-default (Parameterof Integer)]
  [world:paper-width-default (Parameterof Float)]
  [world:column-count-key Symbol]
  [world:column-count-key-default (Parameterof Integer)]
  [world:column-gutter-key Symbol]
  [world:column-gutter-key-default (Parameterof Float)]
  [world:column-index-key Symbol]
  [world:min-first-lines Index]
  [world:min-last-lines Index]
  [world:minimum-lines-per-column Index]
  [world:default-lines-per-column Index]
)
(require/typed/check/provide "measure.rkt"
  [round-float (-> Float Float)]
  [load-text-cache-file (-> Void)]
  [update-text-cache-file (-> Void)]
)
(require/typed/check/provide "utils.rkt"
  (add-vert-positions (-> Quad Quad))
  (attr-change (-> QuadAttrs (Listof Any) QuadAttrs))
  (compute-line-height (-> Quad Quad))
  (hyphenate-quad (USQ -> USQ))
  (join-quads ((Listof Quad) -> (Listof Quad)))
  (merge-attrs (QuadAttrs * -> QuadAttrs))
  (quad-attr-set* (Quad (Listof Any) -> Quad))
  (split-last (All (A) ((Listof A) -> (values (Listof A) A))))
  (split-quad (-> Quad (Listof Quad)))
)
(require/typed/check/provide "sugar-list.rkt"
 (slice-at (All (A) (case-> ((Listof A) Positive-Integer -> (Listof (Listof A)))
                   ((Listof A) Positive-Integer Boolean -> (Listof (Listof A))))))
)
