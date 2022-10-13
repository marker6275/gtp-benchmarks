#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(reprovide (except-in "../base/core-types.rkt"
                      QuadName
                      QuadAttrKey
                      QuadAttrValue
                      QuadAttr
                      QuadAttrs
                      HashableList
                      JoinableType
                      QuadListItem
                      QuadList
                      Treeof
                      Quad
                      GroupQuad
                      GroupQuadListItem
                      GroupQuadList
                      QuadSet
                      Font-Name
                      Font-Size
                      Breakpoint)
           (except-in "../base/quad-types.rkt"
                      BoxQuad
                      RunQuad
                      SpacerQuad
                      DocQuad
                      Optical-KernQuad
                      PieceQuad
                      WordQuad
                      Word-BreakQuad
                      PageQuad
                      Page-BreakQuad
                      ColumnQuad
                      Column-BreakQuad
                      LineQuad
                      BlockQuad
                      Block-BreakQuad))

(provide QuadName
         QuadAttrKey
         QuadAttrValue
         QuadAttr
         QuadAttrs
         HashableList
         JoinableType
         QuadListItem
         QuadList
         Treeof
         Quad
         GroupQuad
         GroupQuadListItem
         GroupQuadList
         QuadSet
         Font-Name
         Font-Size
         Breakpoint

         BoxQuad
         RunQuad
         SpacerQuad
         DocQuad
         Optical-KernQuad
         PieceQuad
         WordQuad
         Word-BreakQuad
         PageQuad
         Page-BreakQuad
         ColumnQuad
         Column-BreakQuad
         LineQuad
         BlockQuad
         Block-BreakQuad)

(define-type QuadName Symbol)
(define-type QuadAttrKey Symbol)
(define-type QuadAttrValue (U Float Index String Symbol Boolean Quad QuadAttrs QuadList Integer))
(define-type QuadAttr (Pairof QuadAttrKey QuadAttrValue))
(define-type QuadAttrs (Listof QuadAttr))
(define-type HashableList  (Rec duo (U Null (List* QuadAttrKey Any duo))))
(define-type JoinableType (U Quad QuadAttrs HashableList))
(define-type QuadListItem (U String Quad))
(define-type QuadList (Listof QuadListItem))
(define-type (Treeof A) (Rec as (U A (Listof as))))
(define-type Quad (List* QuadName QuadAttrs QuadList))
(define-type GroupQuad (List* QuadName QuadAttrs GroupQuadList))
(define-type GroupQuadListItem Quad)
(define-type GroupQuadList (Listof GroupQuadListItem))
(define-type QuadSet (List QuadName QuadAttrs (Listof Quad)))
(define-type Font-Name String)
(define-type Font-Size Positive-Flonum)
(define-type Breakpoint Nonnegative-Integer)

(define-type BoxQuad (List* 'box QuadAttrs QuadList))
(define-type RunQuad (List* 'run QuadAttrs QuadList))
(define-type SpacerQuad (List* 'spacer QuadAttrs QuadList))
(define-type DocQuad (List* 'doc QuadAttrs QuadList))
(define-type Optical-KernQuad (List* 'optical-kern QuadAttrs QuadList))
(define-type PieceQuad (List* 'piece QuadAttrs GroupQuadList))
(define-type WordQuad (List* 'word QuadAttrs QuadList))
(define-type Word-BreakQuad (List* 'word-break QuadAttrs QuadList))
(define-type PageQuad (List* 'page QuadAttrs GroupQuadList))
(define-type Page-BreakQuad (List* 'page-break QuadAttrs QuadList))
(define-type ColumnQuad (List* 'column QuadAttrs GroupQuadList))
(define-type Column-BreakQuad (List* 'column-break QuadAttrs QuadList))
(define-type LineQuad (List* 'line QuadAttrs GroupQuadList))
(define-type BlockQuad (List* 'block QuadAttrs QuadList))
(define-type Block-BreakQuad (List* 'block-break QuadAttrs QuadList))

(require/typed/check/provide "quads.rkt"
  [quads->doc (-> (Listof Quad) DocQuad)]
  (quads->page (-> (Listof Quad) PageQuad))
  (quads->block (-> (Listof Quad) BlockQuad))
  [quad-attrs (Quad -> QuadAttrs)]
  [line (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem LineQuad)]
  [quad-car (-> Quad QuadListItem)]
  [quad-name (-> Quad QuadName)]
  [quad-attr-ref (->* ((U Quad QuadAttrs) QuadAttrKey) (QuadAttrValue) QuadAttrValue)]
  [group-quad-list (-> GroupQuad GroupQuadList)]
  [quad-list (-> Quad QuadList)]
  (quad-has-attr? (-> Quad QuadAttrKey Boolean))
  (quads->column (-> (Listof Quad) ColumnQuad))
  [page (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem PageQuad)]
  [column (->* ((U QuadAttrs HashableList)) () #:rest GroupQuadListItem ColumnQuad)]
)
(require/typed/check/provide "wrap.rkt"
  (insert-spacers-in-line ((LineQuad) ((Option Symbol)) . ->* . LineQuad))
  [wrap-adaptive (->* ((Listof Quad)) (Float) (Listof LineQuad))]
  [wrap-best (->* ((Listof Quad)) (Float) (Listof LineQuad))]
  [wrap-first (->* ((Listof Quad)) (Float) (Listof LineQuad))]
  [fill (->* (LineQuad) ((Option Float)) LineQuad)]
  [add-horiz-positions (-> GroupQuad GroupQuad)])
(require/typed/check/provide "world.rkt"
  [world:line-looseness-key Symbol]
  [world:allow-hyphenated-last-word-in-paragraph Boolean]
  [world:line-looseness-tolerance Float]
  [world:line-index-key Symbol]
  [world:measure-key QuadAttrKey]
  [world:use-hyphenation? Boolean]
  [world:max-quality Index]
  [world:total-lines-key Symbol]
  [world:draft-quality Index]
  [world:quality-default (Parameterof Index)]
  [world:quality-key QuadAttrKey]
  [world:quality-key-default (Parameterof Index)]
  [world:paper-width-default (Parameterof Float)]
  [world:column-count-key QuadAttrKey]
  [world:column-count-key-default (Parameterof Index)]
  [world:column-gutter-key QuadAttrKey]
  [world:column-gutter-key-default (Parameterof Float)]
  [world:column-index-key QuadAttrKey]
  [world:min-first-lines Index]
  [world:min-last-lines Index]
  [world:minimum-lines-per-column Index]
  [world:default-lines-per-column Index])
(require/typed/check/provide "measure.rkt"
  [round-float (-> Float Float)]
  [load-text-cache-file (-> Void)]
  [update-text-cache-file (-> Void)]
)
(require/typed/check/provide "utils.rkt"
  (merge-attrs (-> JoinableType * QuadAttrs))
  (split-last (All (A) (-> (Listof A) (values (Listof A) A))))
  (join-quads (-> (Listof Quad) (Listof Quad)))
  (hyphenate-quad (-> QuadListItem QuadListItem))
  ;; Unused by client
  #;(quad-map (-> (-> QuadListItem QuadListItem) Quad Quad))
  #;(group-quad-attr-set*
   (-> GroupQuad HashableList GroupQuad))
  (quad-attr-set*
   (-> Quad HashableList Quad))
  [attr-change (-> QuadAttrs HashableList QuadAttrs)]
  [compute-line-height (-> Quad Quad)]
  [add-vert-positions (-> GroupQuad GroupQuad)]
  [split-quad (-> Quad (Listof Quad))])
(require/typed/check/provide "sugar-list.rkt"
 (slice-at
  (All (A) (case-> (-> (Listof A) Positive-Integer (Listof (Listof A)))
                   (-> (Listof A) Positive-Integer Boolean (Listof (Listof A)))))))
(require/typed/check/provide "quick-sample.rkt"
  [quick-sample (-> Quad)])
(require/typed/check/provide "render.rkt"
  [pdf-renderer%
    (Class
      [render-to-file (-> Quad Path-String Void)]
      [render-element (-> Quad Any)]
      [render-page (-> (Listof Quad) Void)]
      [render-word (-> Quad Any)]
      [render (-> Quad Any)]
      [finalize (-> Any Any)]
      [setup (-> Quad Quad)]
  )])
