; calendar.

(import :datetime)

(function cal (y m)
  ; cal [[YEAR] MONTH]
  ; Display the calendar for the specified year and month.
  ; If the year and month are omitted, it is considered that the current year and month are specified.
  (let (dt (datetime y m 1) dw (.day-week dt))
    (write-line (format "%d-%02d" y m))
    (write-line  "Su Mo Tu We Th Fr Sa")
    (dotimes (i (-- (* dw 3))) (write-bytes " "))
    (dotimes (i (.monthlen dt))
      (if (!= dw 0) (write-bytes " "))
      (write-bytes (format "%-2d" (++ i)))
      (if (= (<- dw (% (++ dw) 7)) 0)
          (write-line)))))

(function! main (args)
  (let (argc (len args) now (datetime.now))
    (if (= argc 2) (cal (str->num (car args)) (str->num (cadr args)))
        (= argc 1) (cal (.year now) (str->num (car args)))
        (cal (.year now) (.month now)))))
