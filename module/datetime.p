; date and time module.

; Process the date and time of the time zone set in the host system.
; The UTC to local time difference is always calculated to the current value, even during daylight savings times.
; After creating with new, you need to clear and initialize it using the method starting with init.

(class DateTime (Object)
  unix-time year month day day-week hour minute second)

(function datetime.offset-0001-01-01 (y m d)
  ;; Returns the difference date from 0001-01-01 for yyyy-mm-dd.
  (if (<= m 2) (<- y (-- y) m (+ m 12)))
  (+ (* 365 (-- y))
     (// y 4) (- (// y 100)) (// y 400)
     (// (- (* 306 m) 324) 10)
     d -1))

(method DateTime .init (unix-time)
  (let (t nil y nil offset nil)
    (&unix-time! self unix-time)
    ;; utc to localtime and offset from 0001-01-01
    (<- t (+ unix-time (utcoffset) 62135596800))
    (&second! self (% t 60)) (<- t (// t 60))
    (&minute! self (% t 60)) (<- t (// t 60))
    (&hour! self (% t 24)) (<- t (// t 24))
    (&day-week! self (% (+ t 1) 7))    ; 0001-01-01 is Mon
    (<- y (++ (// t 365)))
    (while (> (<- offset (datetime.offset-0001-01-01 y 1 1)) t)
      (<- y (-- y)))
    (&year! self y)
    (&month! self (++ (// (- t offset) 31)))
    (<- offset (datetime.offset-0001-01-01 y (&month self) 1))
    (let (mlen (.monthlen self))
      (when (<= (+ offset mlen) t) 
        (&month! self (++ (&month self)))
        (<- offset (+ offset mlen)))
      (&day! self (+ t (- offset) 1)))
    self))

(function datetime (year month day :opt hour minute second)
  ; Returns the DateTime instance corresponding to the specified argument.
  (.init (.new DateTime)
         ;; day count 1970-01-01
         (+ (* (+ (datetime.offset-0001-01-01 year month day) -719162) 24 60 60)
            (if hour (* hour 60 60) 0)
            (if minute (* minute 60) 0)
            (- (|| second 0) (utcoffset)))))

(function datetime.now ()
  ; Returns a DateTime instance corresponding to the current time.
  (.init (.new DateTime) (time)))

(method DateTime .year ()
  ; Returns the year.
  (&year self))

(method DateTime .month ()
  ; Returns the month (1-12).
  (&month self))

(method DateTime .day ()
  ; Returns the day (1-31).
  (&day self))

(method DateTime .day-week ()
  ; Returns the index of the day of the week (0:sun, 1:mon, ... , 6: sat).
  (&day-week self))

(method DateTime .week-of-month ()
  ; Returns the week of month of the receiver.
  (let (first-day-of-month (datetime (&year self) (&month self) 1))
    (++ (// (+ (&day self) -1 (&day-week first-day-of-month)) 7))))

(method DateTime .hour ()
  ; Returns the hour (0-23).
  (&hour self))

(method DateTime .minute ()
  ; Returns the minute (0-59).
  (&minute self))

(method DateTime .second ()
  ; Returns the second (0-59).
  (&second self))

(method DateTime .unix-time ()
  ; Returns the number of seconds relative to the receiver's unix epoch (January 1, 1970, 00:00:00 UTC).
  (&unix-time self))

(method DateTime .cmp (o)
  (let (x (&unix-time self) y (&unix-time o)) 
    (if (= x y) 0
        (< x y) -1
        1)))

(method DateTime .monthlen ()
  ; Returns the number of days in the year
  (switch (&month self)
    2 (- (datetime.offset-0001-01-01 (&year self) 3 1)
         (datetime.offset-0001-01-01 (&year self) 2 1))
    (4 6 9 11) 30
    :default 31))

(method DateTime .offset (:key days)
  ; Returns an instance at the specified offset from the receiver.
  (let (offset 0)
    (if days (<- offset (* days 60 60 24)))
    (.init (.new DateTime) (+ (.unix-time self) offset))))

(method DateTime .holiday? ()
  ; Returns whether the receiver is Saturday, Sunday, or a public holiday.
  (let (day-week (&day-week self))
    (|| (= day-week 0) (= day-week 6) (.public-holiday? self))))

(method DateTime .public-holiday? ()
  ; Returns whether the receiver is a public holiday.
  ;; https://www8.cao.go.jp/chosei/shukujitsu/gaiyou.html
  ; Do not use for strict judgment due to sloppy construction.
  (let (y (&year self) m (&month self) d (&day self)
          y-1980 (- y 1980) monday? (= (&day-week self) 1) nth-monday (++ (// (-- d) 7)))
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

(method DateTime .date.to-s ()
  (join (map (f (x) (int->str x :padding 2))
             (list (&year self) (&month self) (&day self))) "-"))

(method DateTime .time.to-s ()
  (join (map (f (x) (int->str x :padding 2))
             (list (&hour self) (&minute self) (&second self))) ":"))

(method DateTime .datetime.to-s ()
  (str (.date.to-s self) " " (.time.to-s self)))

(method DateTime .day-week.to-s ()
  (nth (.day-week self) '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))

(method DateTime .to-s ()
  (join (list (.date.to-s self) (.day-week.to-s self) (.time.to-s self)) " "))

(function! main (args)
  (let (dt (.init (.new DateTime) (- 1407737889 (utcoffset))))    ; 2014-08-11 Mon 06:18:09
    (assert (= (.year dt) 2014))
    (assert (= (.month dt) 8))
    (assert (= (.day dt) 11))
    (assert (= (.day-week dt) 1))
    (assert (= (.hour dt) 6))
    (assert (= (.minute dt) 18))
    (assert (= (.second dt) 9))
    (assert (= (.unix-time dt) (- 1407737889 (utcoffset))))
    (assert (.eq? dt dt))
    (assert (! (.eq? dt nil))))
  (let (dt (datetime 2020 08 06 12 10 30))
    (assert (= (.date.to-s dt) "2020-08-06"))
    (assert (= (.time.to-s dt) "12:10:30"))
    (assert (= (.datetime.to-s dt) "2020-08-06 12:10:30"))
    (assert (= (.day-week.to-s dt) "Thu"))
    (assert (= (.to-s dt) "2020-08-06 Thu 12:10:30"))))
