; attendance management.

; # input specification
;
;     INPUT = (MONTH ...)
;     MONTH = (month scheduled-work-days DAY ...)
;     DAY = day start-time-of-work end-time-of-work deduction-time
;
;     ("2021-04" 20
;      ; 2021-04
;      ; Su Mo Tu We Th Fr Sa
;      ;             01 02 03
;      ; 04 05 06 07 08 09 10
;      ; 11 12 13 14 15 16 17
;      ; 18 19 20 21 22 23 24
;      ; 25 26 27 28 29 30
;      1 "10:00" "18:00" 1
;      2 "10:00" "18:00" 1
;      5 "10:00" "18:00" 1
;      7 "10:00" "18:00" 1
;      ...)
;
;      ...
;
; # see also
; coreutils/cal.p

(<- $minimum-operating-time 140)

(function hhmm->min (hhmm)
  (let ((hh mm) (map str->num (split hhmm ":")))
    (+ (* 60 hh) mm)))

(function ->working-hours (expr)
  ; (day start-time-of-work end-time-of-work deduction-time)
  ; -> (day working-hours)
  (let ((day start end deduction-time) expr)
    (list day (/ (- (hhmm->min end) (hhmm->min start) (* 60 deduction-time)) 60))))

(function parse-days (expr)
  (if (!= (% (len expr) 4) 0) (error "illegal format " expr)
      (let (days (map ->working-hours (group expr 4))
                 working-days (len days)
                 working-hours (apply + (map cadr days)))
        (list working-days  working-hours))))

(function format (day hours)
  (list :day day :h hours :h/day (if (= day 0) 'NaN (/ hours day))))

(function parse-month (expr)
  (let ((year-month scheduled-work-days :rest days) expr
        (working-days working-hours) (parse-days days)
        rest-working-days (- scheduled-work-days working-days)
        rest-working-hours (- $minimum-operating-time working-hours))
    (list year-month
          (list :total (format working-days working-hours))
          (list :rest (format rest-working-days rest-working-hours)))))

(function! main (args)
  (foreach write (map parse-month (collect read))))