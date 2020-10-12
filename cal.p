; calendar

(import :datetime)

(<- $usage
"
Usage: paren cal.p [[YEAR] MONTH]
	Display the calendar for the specified year and month.
	If the year and month are omitted, it is considered that the current year and month are specified.
")

(function! main (args)
  (catch (Error (f (e) (write-line $usage) (throw e)))
    (let (argc (length (<- args (cdr args))) dt (DateTime.now) dw nil y nil m nil)
      (if (= argc 0) (<- y (.year dt) m (.month dt))
          (= argc 1) (<- y (.year dt) m (str->num (car args)))
          (= argc 2) (<- y (str->num (car args)) m (str->num (cadr args)))
          (error "too many arguments."))
      (<- dt (DateTime.of y m 1) dw (.day-week dt))
      (write-line (string y "-" m))
      (write-line  "Su Mo Tu We Th Fr Sa")
      (dotimes (i (-- (* dw 3))) (write-mem " "))
      (dotimes (i (.monthlen dt))
        (if (/= dw 0) (write-mem " "))
        (write-mem (int->str (++ i) :padding 2))
        (if (= (<- dw (mod (++ dw) 7)) 0)
            (write-line))))))
