#lang typed/racket/base

(provide
  ;; (struct-out label)
  ;; (struct-out suffix-tree)
  ;; (struct-out node)
 make-node
 make-label
 make-suffix-tree)


(struct label ([datum : (Vectorof (U Char Symbol))] [i : Natural] [j : Natural])
  #:mutable
  #:prefab
  #:type-name Label)
(define make-label label)

;; A suffix tree consists of a root node.
(struct suffix-tree ([root : Node])
  #:prefab
  #:type-name Tree)
(define make-suffix-tree suffix-tree)

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(struct node ([up-label : Label] [parent : (U #f Node)] [children : (Listof Node)] [suffix-link : (U #f Node)])
  #:mutable
  #:prefab
  #:type-name Node)
(define make-node node)
