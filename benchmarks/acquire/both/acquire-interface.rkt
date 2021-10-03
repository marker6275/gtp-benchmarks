#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt"
         "state-adapted.rkt"
         "board-adapted.rkt"
         "../base/types.rkt")

(provide (all-from-out "state-adapted.rkt")
         (all-from-out "board-adapted.rkt"))

(require/typed/check/provide
 "admin.rkt"
 [administrator% Administrator%]
 [turn% Turn%])

(require/typed/check/provide
 "auxiliaries.rkt"
 [randomly-pick (-> (Listof Tile) Tile)])

(require/typed/check/provide
 "basics.rkt"
 [player-shares0 Shares]
 [*combine-shares (-> (Listof Shares) Shares)]
 [shares-minus (-> Shares Shares Shares)]
 [banker-shares0 Shares]
 [ALL-HOTELS (Listof Hotel)]
 [SHARES-PER-TURN# Integer]
 [hotel<=? (-> Hotel Hotel Boolean)]
 [price-per-share (-> Hotel Natural (Option Cash))]
 [shares++ (-> Shares Hotel Shares)]
 [shares-- (-> Shares Hotel Shares)]
 [shares-available (-> Shares Hotel Share)])
