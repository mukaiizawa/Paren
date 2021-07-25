; attendance management.

(<- $minimum-operating-time 140)

(function hhmm->min (hhmm)
  (let ((hh mm) (map int (split hhmm ":")))
    (+ (* 60 hh) mm)))

(function ->working-hours (expr)
  ;; (day start-time-of-work end-time-of-work deduction-time)
  ;; -> (day working-hours)
  (let ((day start end deduction-time) expr)
    (list day (/ (- (hhmm->min end) (hhmm->min start) (* 60 deduction-time)) 60))))

(function parse-days (expr)
  (if (!= (% (len expr) 4) 0) (raise ArgumentError)
      (let (days (map ->working-hours (group expr 4))
                 working-days (len days)
                 working-hours (apply + (map cadr days)))
        (list working-days  working-hours))))

(function parse-month (expr)
  (let ((year-month scheduled-work-days :rest days) expr
        (working-days working-hours) (parse-days days)
        rest-working-days (- scheduled-work-days working-days)
        rest-working-hours (- $minimum-operating-time working-hours)
        format (f (day hours)
                 (list :day day :h hours :h/day (if (= day 0) 'NaN (/ hours day)))))
    (list year-month
          (list :total (format working-days working-hours))
          (list :rest (format rest-working-days rest-working-hours)))))

(function! main (args)
  ; # Synopsis
  ; attendance-management
  ;
  ; # Description
  ; Receive attendance information from standard input and summarize.
  ;
  ; The data received from standard input is the following S-expressions.
  ;
  ;     INPUT = MONTH-EXPR ...
  ;     MONTH-EXPR = (month scheduled-work-days DAY-EXPR ...)
  ;     DAY-EXPR = day start-time-of-work end-time-of-work deduction-time
  ;
  ; # Examples
  ;
  ;     $ cat x
  ;     ("2021-04" 20
  ;      ; 2021-04
  ;      ; Su Mo Tu We Th Fr Sa
  ;      ;              1  2  3
  ;      ;  4  5  6  7  8  9 10
  ;      ; 11 12 13 14 15 16 17
  ;      ; 18 19 20 21 22 23 24
  ;      ; 25 26 27 28 29 30
  ;      1 "10:00" "18:00" 1
  ;      2 "10:00" "18:00" 1
  ;      5 "10:00" "18:00" 1
  ;      7 "10:00" "18:00" 1)
  ;
  ;     $ cat x | paren attendance-management.p
  ;     ("2021-04" (:total (:day 4 :h 28 :h/day 7)) (:rest (:day 16 :h 112 :h/day 7)))
  ;
  ; # See Also
  ; coreutils/cal.p
  ; tool/ldate.p

  (foreach write (map parse-month (collect read))))
