; date and time module.

; Process the date and time of the time zone set in the host system.
; The UTC to local time difference is always calculated to the current value, even during daylight savings times.
; After creating with new, you need to clear and initialize it using the method starting with init.

(class DateTime (Object)
  unix-time year month day day-week hour minute second)

(function DateTime.offset (y m d)
  ; Returns the difference date from 0001-01-01 for yyyy-mm-dd.
  (+ (* 365 (-- y))
     (// y 4) (- (// y 100)) (// y 400)
     (// (- (* 306 m) 324) 10)
     d -1))

(method DateTime .init (unix-time)
  (let (t nil y nil offset nil)
    (&unix-time! self unix-time)
    ;; utc to localtime and offset from 0001-01-01
    (<- t (+ unix-time (utcoffset) 62135596800))
    (&second! self (mod t 60)) (<- t (// t 60))
    (&minute! self (mod t 60)) (<- t (// t 60))
    (&hour! self (mod t 24)) (<- t (// t 24))
    (&day-week! self (mod (+ t 1) 7))    ; 0001-01-01 is Mon
    (<- y (++ (// t 365)))
    (while (> (<- offset (DateTime.offset y 1 1)) t)
      (<- y (-- y)))
    (&year! self y)
    (&month! self (++ (// (- t offset) 31)))
    (<- offset (DateTime.offset y (&month self) 1))
    (let (mlen (.monthlen self))
      (when (<= (+ offset mlen) t) 
        (&month! self (++ (&month self)))
        (<- offset (+ offset mlen)))
      (&day! self (+ t (- offset) 1)))
    self))

(function DateTime.now ()
  (.init (.new DateTime) (time)))

(function DateTime.of (year month day :opt hour minute second)
  (.init (.new DateTime)
         ;; day count 1970-01-01
         (+ (* (+ (DateTime.offset year month day) -719162) 24 60 60)
            (if hour (* hour 60 60) 0)
            (if minute (* minute 60) 0)
            (- (|| second 0) (utcoffset)))))

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

(method DateTime .cmp (o)
  (let (x (&unix-time self) y (&unix-time o)) 
    (if (= x y) 0
        (< x y) -1
        1)))

(method DateTime .monthlen ()
  ; Returns the number of days in the year
  (switch (&month self)
    2 (- (DateTime.offset (&year self) 3 1)
         (DateTime.offset (&year self) 2 1))
    (4 6 9 11) 30
    :default 31))

(method DateTime .date.to-s ()
  (join (map (f (x) (int->str x :padding 2))
             (list (&year self) (&month self) (&day self))) "-"))

(method DateTime .time.to-s ()
  (join (map (f (x) (int->str x :padding 2))
             (list (&hour self) (&minute self) (&second self))) ":"))

(method DateTime .datetime.to-s ()
  (string (.date.to-s self) " " (.time.to-s self)))

(method DateTime .day-week.to-s ()
  (nth '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat") (.day-week self)))

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
  (let (dt (DateTime.of 2020 08 06 12 10 30))
    (assert (memeq? (.date.to-s dt) "2020-08-06"))
    (assert (memeq? (.time.to-s dt) "12:10:30"))
    (assert (memeq? (.datetime.to-s dt) "2020-08-06 12:10:30"))
    (assert (memeq? (.day-week.to-s dt) "Thu"))
    (assert (memeq? (.to-s dt) "2020-08-06 Thu 12:10:30"))))
