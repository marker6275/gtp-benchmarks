#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(require (except-in "typed-data.rkt" make-label))
(provide (all-from-out "typed-data.rkt"))

(require/typed/check/provide
 "label.rkt"
 [make-label (-> (U String (Vectorof (U Char Symbol))) Label)]
 [label-element? (-> Any Boolean)]
 [label-element-equal? (-> Any Any Boolean)]
 [string->label (-> String Label)]
 [string->label/with-sentinel (-> String Label)]
 [vector->label (-> (Vectorof (U Char Symbol)) Label)]
 [vector->label/with-sentinel (-> (Vectorof Char) Label)]
 [label->string (-> Label String)]
 [label->string/removing-sentinel (-> Label String)]
 [label->vector (-> Label (Vectorof (U Char Symbol)))]
 [label-length (-> Label Index)]
 [label-ref (-> Label Integer (U Symbol Char))]
 [sublabel (case-> (-> Label Index Label)
                   (-> Label Index Index Label))]
 [sublabel! (case-> (-> Label Index Void)
                    (-> Label Index Index Void))]
 [label-prefix? (-> Label Label Boolean)]
 [label-equal? (-> Label Label Boolean)]
 [label-empty? (-> Label Boolean)]
 [label-copy (-> Label Label)]
 [label-ref-at-end? (-> Label Integer Boolean)]
 [label-source-id (-> Label Integer)]
 [label-same-source? (-> Label Label Boolean)]
 [label-source-eq? (-> Label Label Boolean)])

(require/typed/check/provide
 "structs.rkt"
 [tree? (Any -> Boolean)]
 [tree-root (Tree -> Node)]
 [make-tree (-> Tree)]
 [new-suffix-tree (-> Tree)]
 [node-find-child (-> Node Any (U Node #f))]
 [node-root? (-> Node Boolean)]
 [node-position-at-end? (-> Node Index Boolean)]
 [node-add-leaf! (-> Node Label Node)]
 [node-up-splice-leaf! (-> Node Index Label (values Node Node))]
 [node-follow/k (-> Node
                    Label
                    (-> Node (Pairof Node Index))
                    (-> Node Index (Pairof Node Index))
                    (-> Node Label Index (Pairof Node Index))
                    (-> Node Index Label Index (Pairof Node Index))
                    (Pairof Node Index))])

(require/typed/check/provide
 "ukkonen.rkt"
 [skip-count (-> Node Label (values Node Index))]
 [jump-to-suffix (-> Node (values Node (U Boolean Integer)))]
 [try-to-set-suffix-edge! (-> Node Node Void)]
 [find-next-extension-point/add-suffix-link! (-> Node Label Index Index (values (U #f Node) (U #f Index) (U #f Index)))]
 [extend-at-point! (-> Node Index Label Index Node)]
 [suffix-tree-add! (-> Tree Label Void)]
 [tree-add! (-> Tree Label Void)])

