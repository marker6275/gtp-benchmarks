#lang racket/base

(require
  "core-structs.rkt")

(provide
  (struct-out Date)
  (struct-out Time)
  (struct-out DateTime)
  (struct-out Moment))

;; Structs from the main gregor modules
;; `date.rkt`, `time.rkt`, `datetime.rkt`, `moment-base.rkt`

(struct Date (ymd ;: YMD]
              jdn ;: Integer]))
              )
  #:prefab)

(struct Time (hmsn ;: HMSN]
              ns ;: Natural]))
)
  #:prefab)

(struct DateTime (date ;: Date]
                  time ;: Time]
                  jd ;: Exact-Rational]))
)
  #:prefab)

(struct Moment (datetime/local ;: DateTime]
                utc-offset ;: Integer]
                zone ;: (U String #f)]))
)
  #:prefab)
