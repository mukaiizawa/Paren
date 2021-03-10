; calendar.

(import :datetime)

(function cal (y m)
  ; cal [[YEAR] MONTH]
  ; Display the calendar for the specified year and month.
  ; If the year and month are omitted, it is considered that the current year and month are specified.
  (let (dt (DateTime.of y m 1) dw (.day-week dt))
    (write-line (string y "-" (int->str m :padding 2)))
    (write-line  "Su Mo Tu We Th Fr Sa")
    (dotimes (i (-- (* dw 3))) (write-mem " "))
    (dotimes (i (.monthlen dt))
      (if (!= dw 0) (write-mem " "))
      (write-mem (int->str (++ i) :padding 2))
      (if (= (<- dw (mod (++ dw) 7)) 0)
          (write-line)))))

(function! main (args)
  (let (argc (length args) now (DateTime.now))
    (if (= argc 2) (cal (str->num (car args)) (str->num (cadr args)))
        (= argc 1) (cal (.year now) (str->num (car args)))
        (cal (.year now) (.month now)))))
