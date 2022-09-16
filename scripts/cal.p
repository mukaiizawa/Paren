; calendar.

(import :datetime)

(function cal (y m)
  (let (dt (datetime y m 1) dw (.day-week dt))
    (printf "%d-%02d\n" y m)
    (println  "Su Mo Tu We Th Fr Sa")
    (dotimes (i (-- (* dw 3))) (print " "))
    (dotimes (i (.monthlen dt))
      (if (!= dw 0) (print " "))
      (printf "%2d" (++ i))
      (if (= (<- dw (% (++ dw) 7)) 0)
          (println)))
    (println)))

(function! main (args)
  (let (argc (len args) now (datetime.now))
    (if (= argc 2) (cal (int (car args)) (int (cadr args)))
        (= argc 1) (cal (.year now) (int (car args)))
        (cal (.year now) (.month now)))))
