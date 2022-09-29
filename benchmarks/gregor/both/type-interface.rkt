#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt"
         "../base/types.rkt")

(reprovide "../base/types.rkt"
           ;; "gregor-structs-adapter.rkt"
           "tzinfo-adapter.rkt"
)
(struct YMD ([y : Natural]
             [m : Month]
             [d : Natural]) #:prefab)
(struct HMSN ([h : Integer]
              [m : Integer]
              [s : Integer]
              [n : Integer]) #:prefab)
(struct Date ([ymd : YMD]
              [jdn : Integer])
  #:prefab)

(struct Time ([hmsn : HMSN] [ns : Natural])
  #:prefab)

(struct DateTime ([date : Date]
                  [time : Time]
                  [jd : Exact-Rational])
  #:prefab)

(struct Moment ([datetime/local : DateTime]
                [utc-offset : Integer]
                [zone : (U String #f)])
  #:prefab)
(provide (struct-out YMD)
         (struct-out HMSN)
         (struct-out Date)
         (struct-out Time)
         (struct-out DateTime)
         (struct-out Moment))
(require/typed/check/provide "date.rkt"
    [date=? (-> Date Date Boolean)]
    [date (->* (Natural) (Month Natural) Date)]
    [date->iso8601 (-> Date String)]
)
(require/typed/check/provide "time.rkt"
    [time=? (-> Time Time Boolean)]
    [time->iso8601 (-> Time String)]
    [make-time (->* (Integer) (Integer Integer Integer) Time)]
)
(require/typed/check/provide "datetime.rkt"
    [datetime=? (-> DateTime DateTime Boolean)]
    [datetime<=? (-> DateTime DateTime Boolean)]
    [datetime (->* (Natural) (Month Natural Natural Natural Natural Natural) DateTime)]
    [datetime->time (-> DateTime Time)]
    [datetime->date (-> DateTime Date)]
    [datetime->iso8601 (-> DateTime String)]
    [datetime->posix (-> DateTime Exact-Rational)]
)
(require/typed/check/provide "moment.rkt"
    [current-timezone (Parameterof (U tz #f))]
    [moment (->* (Natural) (Month Natural Natural Natural Natural Natural #:tz (U tz #f) #:resolve-offset (-> (U tzgap tzoverlap) DateTime (U String #f) (U #f Moment) Moment)) Moment)]
    [moment=? (-> Moment Moment Boolean)]
    [UTC String]
    [moment->iso8601/tzid (-> Moment String)]
    [posix->moment (-> Exact-Rational tz Moment)]
)
(require/typed/check/provide "clock.rkt"
    [current-clock (Parameterof (-> Exact-Rational))]
    [today/utc (-> Date)]
    [today (->* () (#:tz (U tz #f)) Date)]
    [current-time/utc (-> Time)]
    [current-time (->* () (#:tz (U tz #f)) Time)]
    [now/utc (-> DateTime)]
    [now (->* () (#:tz (U tz #f)) DateTime)]
    [now/moment/utc (-> Moment)]
    [now/moment (-> Moment)]
)
(require/typed/check/provide "difference.rkt"
    [datetime-months-between (-> DateTime DateTime Integer)]
    [datetime-days-between (-> DateTime DateTime Integer)]
    [datetime-nanoseconds-between (-> DateTime DateTime Integer)]
)
