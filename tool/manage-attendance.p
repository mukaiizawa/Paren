; manage attendance.

(<- $lower-limit 140
    $default-deduction-time 1)

(function hhmm->min (hhmm)
  (let ((hh mm) (map int (split hhmm ":")))
    (+ (* 60 hh) mm)))

(function parse-day (expr)
  ;; (day start end deduction-time) -> (day working-hour)
  (let ((day start end :opt deduction-time) expr)
    (list day (/ (- (hhmm->min end) (hhmm->min start) (* 60 (|| deduction-time $default-deduction-time))) 60))))

(function parse-days (days)
  (let (pairs (map parse-day days))
    (list (len pairs) (apply + (map cadr pairs)))))

(function more-zero? (x)
  (&& x (> x 0)))

(function summarize (title day hour)
  (if (more-zero? day) (list title (list :day day :h hour :h/day (/ hour day)))))

(function parse (expr)
  (catch (Error (f (e) nil))
    (let (((year month :opt scheduled-working-days)
           :rest days) expr
          (working-days working-hours) (parse-days days)
          rest-working-days (&& scheduled-working-days (- scheduled-working-days working-days))
          rest-working-hours (- $lower-limit working-hours))
      `(:year ,year :month ,month
              ,@(summarize :total working-days working-hours)
              ,@(summarize :rest rest-working-days rest-working-hours)))))

(function! main (args)
  (foreach write (map parse (collect read))))
