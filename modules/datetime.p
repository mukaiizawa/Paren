; date and time module.

(class DateTime (Object)
  unix-time year month day hour minute second utc-offset)

(method DateTime .init (unix-time :opt utc-offset)
  ; Build an instance corresponding to unix-time.
  ; If utc-offset is specified, build an instance in a time zone that is off by the specified number of seconds from the UTC.
  ; Otherwise, the host's utcoffset is used.
  (let (t nil ordinal nil days nil)
    (<- self->unix-time unix-time
        self->utc-offset (|| utc-offset (utcoffset))
        t (+ unix-time 62135683200 self->utc-offset)    ; 0001-01-01
        self->second (% t 60) t (// t 60)
        self->minute (% t 60) t (// t 60)
        self->hour (% t 24) t (// t 24)
        self->year (++ (// t 365)))
    (while (> (<- ordinal (datetime.ordinal self->year 1 1)) t)
      (<- self->year (-- self->year)))
    (<- self->month (++ (// (- t ordinal) 31))
        days (.days-in-month self)
        ordinal (datetime.ordinal self->year self->month 1))
    (when (<= (+ ordinal days) t)
      (<- self->month (++ self->month)
          ordinal (+ ordinal days)))
    (<- self->day (+ t (- ordinal) 1))
    self))

(method DateTime .ordinal ()
  (datetime.ordinal self->year self->month self->day))

(method DateTime .year ()
  ; Returns the year.
  self->year)

(method DateTime .leap-year? ()
  (!= (- (datetime.ordinal (++ self->year) 1 1)
         (datetime.ordinal self->year 1 1))
      365))

(method DateTime .month ()
  ; Returns the month (1-12).
  self->month)

(method DateTime .days-in-month ()
  (- (if (= self->month 12) (datetime.ordinal (++ self->year) 1 1)
         (datetime.ordinal self->year (++ self->month) 1))
     (datetime.ordinal self->year self->month 1)))

(method DateTime .day ()
  ; Returns the day (1-31).
  self->day)

(method DateTime .day-week ()
  ; Returns the index of the day of the week (0:sun, 1:mon, ... , 6: sat).
  (% (.ordinal self) 7))

(method DateTime .week-of-month ()
  ; Returns the week of month of the receiver.
  (++ (// (+ (.day self) -1 (.day-week (datetime self->year self->month 1))) 7)))

(method DateTime .hour ()
  ; Returns the hour (0-23).
  self->hour)

(method DateTime .minute ()
  ; Returns the minute (0-59).
  self->minute)

(method DateTime .second ()
  ; Returns the second (0-59).
  self->second)

(method DateTime .unix-time ()
  ; Returns the number of seconds relative to the receiver's unix epoch (January 1, 1970, 00:00:00 UTC).
  self->unix-time)

(method DateTime .msdos-date ()
  ; Returns the msdos date.
  (| (<< (- (.year self) 1980) 9)
     (| (<< (.month self) 5)
        (.day self))))

(method DateTime .msdos-time ()
  ; Returns the msdos time.
  (| (<< (.hour self) 11)
     (| (<< (.minute self) 5)
        (// (.second self) 2))))

(method DateTime .offset (:key weeks days hours minutes seconds)
  ; Returns an instance at the specified offset from the receiver.
  (if weeks (<- days (+ (|| days 0) (* weeks 7))))
  (if days (<- hours (+ (|| hours 0) (* days 24))))
  (if hours (<- minutes (+ (|| minutes 0) (* hours 60))))
  (if minutes (<- seconds (+ (|| seconds 0) (* minutes 60))))
  (if seconds (datetime.from-unix-time (+ (.unix-time self) seconds)) self))

(method DateTime .tomorrow () (.offset self :days 1))
(method DateTime .yesterday () (.offset self :days -1))

(method DateTime .sunday? () (= (.day-week self) 0))
(method DateTime .monday? () (= (.day-week self) 1))
(method DateTime .tuesday? () (= (.day-week self) 2))
(method DateTime .wednesday? () (= (.day-week self) 3))
(method DateTime .thursday? () (= (.day-week self) 4))
(method DateTime .friday? () (= (.day-week self) 5))
(method DateTime .saturday? () (= (.day-week self) 6))
(method DateTime .weekend? () (|| (.sunday? self) (.saturday? self)))

(method DateTime .holiday? ()
  ; Returns whether the receiver is Saturday, Sunday, or a public holiday.
  (|| (.weekend? self) (.public-holiday? self)))

(method DateTime .public-holiday? ()
  ; Returns whether the receiver is a public holiday.
  ;; https://www8.cao.go.jp/chosei/shukujitsu/gaiyou.html
  ; Do not use for strict judgment due to sloppy construction.
  (let (y self->year m self->month d self->day
          y-1980 (- y 1980) monday? (.monday? self) nth-monday (++ (// (-- d) 7)))
    (|| (&& (= m 1) (= d 1))
        (&& (= m 1) monday? (= nth-monday 2))
        (&& (= m 2) (= d 11))
        (&& (= m 3) (= d (// (+ 20.8431 (* 0.242194 y-1980) (// y-1980 -4)))))
        (&& (= m 4) (= d 29))
        (&& (= m 5) (= d 3))
        (&& (= m 5) (= d 4))
        (&& (= m 5) (= d 5))
        (&& (= m 7) monday? (= nth-monday 3))
        (&& (= m 8) (= d 11))
        (&& (= m 9) monday? (= nth-monday 3))
        (&& (= m 9) (= d (// (+ 23.2488 (* 0.242194 y-1980) (// y-1980 -4)))))
        (&& (= m 10) monday? (= nth-monday 2))
        (&& (= m 11) (= d 3))
        (&& (= m 11) (= d 23))
        (|| (&& (< y 1989) (= m 4) (= d 29))
            (&& (< y 2019) (= m 12) (= d 23))
            (&& (= m 2) (= d 23)))
        (&& monday? (.public-holiday? (.offset self :days -1))))))

(method DateTime .to-s.date ()
  (format "%d-%02d-%02d" self->year self->month self->day))

(method DateTime .to-s.time ()
  (format "%02d:%02d:%02d" self->hour self->minute self->second))

(method DateTime .to-s.datetime ()
  (str (.to-s.date self) " " (.to-s.time self)))

(method DateTime .to-s.month ()
  ([] '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") (-- (.month self))))

(method DateTime .to-s.day-week ()
  ([] '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat") (.day-week self)))

(method DateTime .to-s ()
  (join (list (.to-s.date self) (.to-s.day-week self) (.to-s.time self)) " "))

;; API.

(function datetime (year month day :opt hour minute second utc-offset)
  ; Returns the DateTime instance corresponding to the specified argument.
  (datetime.from-unix-time
    (+ (* (- (datetime.ordinal year month day) 719163) 24 60 60)
       (if hour (* hour 60 60) 0)
       (if minute (* minute 60) 0)
       (|| second 0)
       (- (|| utc-offset (utcoffset))))
    utc-offset))

(function datetime.from-unix-time (unix-time :opt utc-offset)
  (.init (.new DateTime) unix-time utc-offset))

(function datetime.from-ordinal (ordinal)
  (datetime.from-unix-time (* (- ordinal 719163) 24 60 60)))

(function datetime.ordinal (year month day)
  ; Return proleptic Gregorian ordinal for the year, month and day.
  ; January 1 of year 1 is day 1.
  ; based on Zeller's congruence.
  (if (! (<= 0 year)) (raise ArgumentError "invalid year `%d`" month)
      (! (<= 1 month 12)) (raise ArgumentError "invalid month `%d`" month)
      (! (<= 0 day 31)) (raise ArgumentError "invalid day `%d`" day)
      (<= month 2) (<- year (-- year) month (+ month 12)))
  (+ (* 365 year) (// year 4) (- (// year 100)) (// year 400)
     (// (* 306 (++ month)) 10)
     (- day 428)))

(function datetime.parse-msdos-datetime (date time)
  ; Returns the DateTime instance corresponding to the msdos date/time.
  (datetime (+ (>> date 9) 1980)
            (& (>> date 5) 0xf)
            (& date 0x1f)
            (& (>> time 11) 0x1f)
            (& (>> time 5) 0x3f)
            (* (& time 0x1f) 2)
            (utcoffset)))

(function datetime.now (:opt utc-offset)
  ; Returns a DateTime instance corresponding to the current time of the time zone set in the host system.
  (datetime.from-unix-time (time) utc-offset))

(function! main (args)
  (assert (= (.ordinal (datetime 1 1 1)) 1))
  (assert (= (.ordinal (datetime 1970 1 1)) 719163))
  (assert (= (.year (datetime 1900 1 1)) 1900))
  (assert (= (.year (datetime 1901 1 1)) 1901))
  (assert (= (.year (datetime 1904 1 1)) 1904))
  (assert (= (.year (datetime 2000 1 1)) 2000))
  (assert (! (.leap-year? (datetime 1900 1 1))))
  (assert (! (.leap-year? (datetime 1901 1 1))))
  (assert (.leap-year? (datetime 1904 1 1)))
  (assert (.leap-year? (datetime 2000 1 1)))
  (dotimes (i 12)
    (assert (= (.month (datetime 1970 (++ i) 1)) (++ i))))
  (assert (= (.days-in-month (datetime 1900 1 1)) 31))
  (assert (= (.days-in-month (datetime 1900 2 1)) 28))
  (assert (= (.days-in-month (datetime 2000 2 1)) 29))
  (assert (= (.days-in-month (datetime 1900 3 1)) 31))
  (assert (= (.days-in-month (datetime 1900 4 1)) 30))
  (assert (= (.days-in-month (datetime 1900 5 1)) 31))
  (assert (= (.days-in-month (datetime 1900 6 1)) 30))
  (assert (= (.days-in-month (datetime 1900 7 1)) 31))
  (assert (= (.days-in-month (datetime 1900 8 1)) 31))
  (assert (= (.days-in-month (datetime 1900 9 1)) 30))
  (assert (= (.days-in-month (datetime 1900 10 1)) 31))
  (assert (= (.days-in-month (datetime 1900 11 1)) 30))
  (assert (= (.days-in-month (datetime 1900 12 1)) 31))
  (dotimes (i 31)
    (assert (= (.day (datetime 1970 1 (++ i))) (++ i))))
  (assert (= (* (datetime.ordinal 1970 1 1) 24 60 60) 62135683200))
  (assert (= (.tomorrow (datetime 1970 1 1)) (datetime 1970 1 2)))
  (assert (= (.yesterday (datetime 1970 2 1)) (datetime 1970 1 31)))
  (assert (= (.offset (datetime 1970 1 1) :days 31 :hours 1 :minutes 1 :seconds 1) (datetime 1970 2 1 1 1 1)))
  ; (assert (= (datetime.from-ordinal 719163) (datetime 1970 1 1)))
  (assert (= (datetime.ordinal 1 1 1) 1))
  (assert (= (datetime.ordinal 1970 1 1) 719163))
  (let (dt (datetime.from-unix-time 1407737889 0))    ; 2014-08-11 Mon 06:18:09 UTC
    (assert (= (.year dt) 2014))
    (assert (= (.month dt) 8))
    (assert (= (.days-in-month dt) 31))
    (assert (= (.day dt) 11))
    (assert (= (.day-week dt) 1))
    (assert (.monday? dt))
    (assert (= (.week-of-month dt) 3))
    (assert (= (.hour dt) 6))
    (assert (= (.minute dt) 18))
    (assert (= (.second dt) 9))
    (assert (= (.unix-time dt) 1407737889)))
  (let (dt (datetime.from-unix-time 1407737889 (* 9 60 60)))    ; 2014-08-11 Mon 15:18:09 JST
    (assert (= (.year dt) 2014))
    (assert (= (.month dt) 8))
    (assert (= (.days-in-month dt) 31))
    (assert (= (.day dt) 11))
    (assert (= (.day-week dt) 1))
    (assert (.monday? dt))
    (assert (= (.week-of-month dt) 3))
    (assert (= (.hour dt) 15))
    (assert (= (.minute dt) 18))
    (assert (= (.second dt) 9))
    (assert (= (.unix-time dt) 1407737889))
    (let (msdt (datetime.parse-msdos-datetime (.msdos-date dt) (.msdos-time dt)))
      (assert (= (.year dt) (.year msdt)))
      (assert (= (.month dt) (.month msdt)))
      (assert (= (.day dt) (.day msdt)))
      (assert (= (.hour dt) (.hour msdt)))
      (assert (= (.minute dt) (.minute msdt)))
      (assert (= (.second dt) (++ (.second msdt))))))
  (let (dt (datetime 2020 8 6 12 10 30))
    (assert (! (.weekend? dt)))
    (assert (= (.to-s.date dt) "2020-08-06"))
    (assert (= (.to-s.time dt) "12:10:30"))
    (assert (= (.to-s.datetime dt) "2020-08-06 12:10:30"))
    (assert (= (.to-s.day-week dt) "Thu"))
    (assert (= (.to-s.month dt) "August"))
    (assert (= (.to-s dt) "2020-08-06 Thu 12:10:30")))
  (assert (= (.unix-time (datetime 2020 8 6 12 10 30 0))
             (.unix-time (datetime 2020 8 6 21 10 30 (* 9 60 60))))))
