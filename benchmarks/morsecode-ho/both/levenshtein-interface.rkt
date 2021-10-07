#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(require/typed/check/provide "levenshtein.rkt"
  [list-levenshtein/predicate (-> (Listof Char) (Listof Char) (-> Char Char Boolean) Index)])
