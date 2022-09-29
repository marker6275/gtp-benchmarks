#lang typed/racket/base

;; (require
;;   "core-adapter.rkt")

;; (provide
;;   (struct-out Date)
;;   (struct-out Time)
;;   (struct-out DateTime)
;;   (struct-out Moment))

;; ;; Structs from the main gregor modules
;; ;; `date.rkt`, `time.rkt`, `datetime.rkt`, `moment-base.rkt`

;; (struct YMD ([y : Natural]
;;              [m : Month]
;;              [d : Natural]) #:prefab)
;; (struct HMSN ([h : Integer]
;;               [m : Integer]
;;               [s : Integer]
;;               [n : Integer]) #:prefab)
;; (struct Date ([ymd : YMD]
;;               [jdn : Integer])
;;   #:prefab)

;; (struct Time ([hmsn : HMSN] [ns : Natural])
;;   #:prefab)

;; (struct DateTime ([date : Date]
;;                   [time : Time]
;;                   [jd : Exact-Rational])
;;   #:prefab)

;; (struct Moment ([datetime/local : DateTime]
;;                 [utc-offset : Integer]
;;                 [zone : (U String #f)])
;;   #:prefab)

