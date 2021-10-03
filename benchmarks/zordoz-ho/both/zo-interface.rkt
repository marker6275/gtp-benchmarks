#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt"
         "../base/typed-zo-structs.rkt")

(require/typed/check/provide
 "zo-string.rkt"
 [zo->spec (-> zo Spec)]
 [zo->string (->* (zo) (#:deep? Boolean) String)])
(require/typed/check/provide
 "zo-transition.rkt"
 [zo-transition (-> zo String (values (U zo (Listof zo)) Boolean))])
(require/typed/check/provide
 "zo-find.rkt"
 [zo-find (-> zo (-> String Boolean) [#:limit (U Natural #f)] (Listof result))]
 [#:struct result ([zo : zo]
                   [path : (Listof zo)])])
