; list date.

(import :datetime)
(import :optparse)

(function ldate (y m :key long?)
  ; ldate [OPTION] [[YEAR] MONTH]
  ; List date.
  ; If the year and month are omitted, it is considered that the current year and month are specified.
  ;     -l use long listing format
  (let (dt (datetime y m 1))
    (dotimes (i (.monthlen dt))
      (write-bytes (.date.to-s dt))
      (if long?  (write-line (str " " (.day-week.to-s dt)))
          (write-line))
      (<- dt (.offset dt :days 1)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "l") args)
                  long? (.get op "l")
                  argc (len args)
                  now (datetime.now))
    (if (= argc 2) (ldate (str->num (car args)) (str->num (cadr args)) :long? long?)
        (= argc 1) (ldate (.year now) (str->num (car args)) :long? long?)
        (ldate (.year now) (.month now) :long? long?))))
