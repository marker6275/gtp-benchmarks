#lang racket/base

(provide
  ;; (struct-out label)
  ;; (struct-out suffix-tree)
  ;; (struct-out node)
  make-node
  make-label
  make-suffix-tree)


(struct label (datum i j)
  #:mutable
  #:prefab)
(define make-label label)

;; A suffix tree consists of a root node.
(struct suffix-tree (root)
  #:prefab)
(define make-suffix-tree suffix-tree)

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(struct node (up-label parent children suffix-link)
  #:mutable
  #:prefab)
(define make-node node)
