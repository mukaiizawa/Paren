; manage attendance.

(import :optparse)

(<- $target-working-hours 140
    $default-deduction-time 1
    $verbose? nil)

(function verbose-write (x)
  (if (nil? $verbose?) x
      (write x)))

(function min->hour (min)
  (/ min 60))

(function hour->min (hour)
  (* hour 60))

(function hhmm->min (hhmm)
  (let ((hh mm) (map int (split hhmm ":")))
    (+ (hour->min hh) mm)))

(function day-expr->working-hour (day-expr)
  (let ((day start end :opt deduction-time) day-expr
        working-hour (min->hour (- (hhmm->min end) (hhmm->min start)
                                   (hour->min (|| deduction-time $default-deduction-time)))))
    (verbose-write (list :year (dynamic year) :month (dynamic month) :day day :hour working-hour))
    working-hour))

(function summarize (title day hour)
  (if (&& day (pos? day)) (list title (list :day day :h hour :h/day (/ hour day)))))

(function parse (expr)
  (let (((year month :opt scheduled-working-days) :rest day-exprs) expr
        working-days (len day-exprs)
        working-hours (apply + (map day-expr->working-hour day-exprs))
        rest-working-days (&& scheduled-working-days (- scheduled-working-days working-days))
        rest-working-hours (- $target-working-hours working-hours))
    `(:year ,year :month ,month
            ,@(summarize :total working-days working-hours)
            ,@(summarize :rest rest-working-days rest-working-hours))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "h:v") args))
    (<- $verbose? (.get op "v"))
    (<- $target-working-hours (int (|| (.get op "h") 140)))
    (foreach write (map parse (collect read)))))
