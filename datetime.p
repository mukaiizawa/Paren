; date and time

; Process the date and time of the time zone set in the host system.
; The UTC to local time difference is always calculated to the current value, even during daylight savings times.
; After creating with new, you need to clear and initialize it using the method starting with init.
; Valid from 1904 to 2099.

(class DateTime (Object Comparable)
  unix-time year month day day-week hour minute second)

(method DateTime .init (unix-time)
  (let (t nil)
    (&unix-time<- self unix-time)
    (<- t (+ unix-time (utcoffset))    ; to localtime.
        t (+ t (* 24107 3600 24)))    ; to 1970-01-01.
    (&second<- self (mod t 60))
    (<- t (// t 60))
    (&minute<- self (mod t 60))
    (<- t (// t 60))
    (&hour<- self (mod t 24))
    (<- t (// t 24))
    (&day-week<- self (mod (+ t 5) 7))    ; 1904-01-01 is friday
    (&year<- self (+ (* (// t 1461) 4) 1904))    ; (+ (* 365 3) 366)
    (<- t (mod t 1461))
    (while (>= t (.yearlen self))
      (<- t (- t (.yearlen self)))
      (&year<- self (++ (&year self))))
    (&month<- self 1)
    (while (>= t (.monthlen self))
      (<- t (- t (.monthlen self)))
      (&month<- self (++ (&month self))))
    (&day<- self (++ t))
    self))

(function DateTime.now ()
  (.init (.new DateTime) (time)))

(function DateTime.of (year month day :opt hour minute second)
  (let (dt (.new DateTime)
           hour (|| hour 0) minute (|| minute 0) second (|| second 0)
           y4 (// (- year 1904) 4) t (* y4 1461))
    (assert (<= 1904 year 2099))
    (&year<- dt (+ 1904 (* y4 4)))
    (<- t (* y4 1461))
    (while (< (&year dt) year)
      (<- t (+ t (.yearlen dt)))
      (&year<- dt (++ (&year dt))))
    (&month<- dt 1)
    (while (< (&month dt) month)
      (<- t (+ t (.monthlen dt)))
      (&month<- dt (++ (&month dt))))
    (&day<- dt day)
    (<- t (-- (+ t day)))
    (&day-week<- dt (mod (+ t 5) 7))    ; 1904-01-01 is friday
    (&hour<- dt hour)
    (<- t (+ (* t 24) hour))
    (&minute<- dt minute)
    (<- t (+ (* t 60) minute))
    (&second<- dt second)
    (<- t (+ (* t 60) second)
        t (- t (* 24107 3600 24)))
    (&unix-time<- dt (- t (utcoffset)))))


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

(method DateTime .eq? (o)
  (&& (is-a? o DateTime) (= (&unix-time self) (.unix-time o))))

(method DateTime .lt? (:rest args)
  (apply < (map .unix-time (cons self args))))

(method DateTime .leap-year? ()
  (= (mod (&year self) 4) 0))

(method DateTime .yearlen ()
  ; Returns the number of days in the year
  (if (.leap-year? self) 366
      365))

(method DateTime .monthlen ()
  ; Returns the number of days in the year
  (switch (&month self)
    2 (if (.leap-year? self) 29 28)
    (4 6 9 11) 30
    :default 31))

(function DateTime.pad2 (i)
  (if (< i 10) (string "0" i)
      (string i)))

(method DateTime .date.to-s ()
  (list->string (map DateTime.pad2
                     (list (&year self) (&month self) (&day self)))
                "-"))

(method DateTime .time.to-s ()
  (list->string (map DateTime.pad2
                     (list (&hour self) (&minute self) (&second self)))
                ":"))

(method DateTime .datetime.to-s ()
  (string (.date.to-s self) " " (.time.to-s self)))

(method DateTime .day-week.to-s ()
  (nth '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat") (.day-week self)))

(method DateTime .to-s ()
  (list->string (list (.date.to-s self) (.day-week.to-s self) (.time.to-s self)) " "))

(function! main ()
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
    (assert (! (.eq? dt nil)))
    (assert (.lt? dt (DateTime.now))))
  (let (dt (DateTime.of 2020 08 06 12 10 30))
    (assert (string= (.date.to-s dt) "2020-08-06"))
    (assert (string= (.time.to-s dt) "12:10:30"))
    (assert (string= (.datetime.to-s dt) "2020-08-06 12:10:30"))
    (assert (string= (.day-week.to-s dt) "Thu"))
    (assert (string= (.to-s dt) "2020-08-06 Thu 12:10:30"))))
