; list date.

(import :datetime)
(import :optparse)

(function ldate (y m :key long?)
  (let (dt (datetime y m 1))
    (dotimes (i (.monthlen dt))
      (if (nil? long?) (println (.to-s.date dt))
          (println (.to-s.date dt) " " (.to-s.day-week dt)))
      (<- dt (.offset dt :days 1)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "l") args)
                  long? (.get op "l")
                  argc (len args)
                  now (datetime.now))
    (if (= argc 2) (ldate (int (car args)) (int (cadr args)) :long? long?)
        (= argc 1) (ldate (.year now) (int (car args)) :long? long?)
        (ldate (.year now) (.month now) :long? long?))))
