; attendance management system.

(import :datetime)
(import :optparse)

(<- $default-deduction-time 1)

(function cadr-member (key lis)
  (cadr (member key lis)))

(function write-verbosely (x)
  (if (nil? $verbose?) x
      (write x)))

(function write-summary (x)
  (if (! $suppress-summary?) (write x))
  (when (&& $estimate? (in? :total x) (in? :rest x))
    (let (h/day (|| $estimate-with (cadr-member :h/day (cadr-member :total x)))
                rest (cadr-member :rest x)
                day (cadr-member :day rest)
                h (cadr-member :h rest))
      (while (pos? day)
        (write (list :days day :h h :h/day (/ h day)))
        (<- day (-- day)
            h (- h h/day))))))

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
                                       (hour->min (<- deduction-time (|| deduction-time $default-deduction-time))))))
    (write-verbosely `(:year ,(dynamic year) :month ,(dynamic month) :day ,day
                             ,@(if $long? `(:start ,start :end ,end :deduction ,deduction-time))
                             :hour ,working-hour))
    working-hour))

(function summarize (title day hour)
  (if (&& day (pos? day)) (list title (list :day day :h hour :h/day (/ hour day)))))

(function parse1 (expr)
  (let (((year month :opt scheduled-working-days) :rest day-exprs) expr
        working-days (len day-exprs)
        working-hours (apply + (map day-expr->working-hour day-exprs))
        rest-working-days (&& scheduled-working-days (- scheduled-working-days working-days))
        rest-working-hours (- $target-working-hours working-hours))
    `(:year ,year :month ,month
            ,@(summarize :total working-days working-hours)
            ,@(summarize :rest rest-working-days rest-working-hours))))

(function read1 ()
  (let (expr (read) year (caar expr) month (cadar expr))
    (if (nil? expr) nil
        (&& (|| (nil? $year) (in? year $year))
            (|| (nil? $month) (in? month $month))) expr
        (read1))))

(function parse ()
  (foreach (compose write-summary parse1)
           (collect read1)))

(function show-table ()
  (let (lo 120 hi 200 dh 10)
    ;; header
    (for (h lo) (<= h hi) (h (+ h dh))
      (if (= h lo) (print "  "))
      (printf " %6d" h)
      (if (= h hi) (println)))
    ;; body
    (for (days 1) (<= days 31) (days (++ days))
      (printf "%2d" days)
      (for (h lo) (<= h hi) (h (+ h dh))
        (printf " %6.2f" (/ h days)))
      (println))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "aeE:m:h:lsty:v") args) now (datetime.now))
    (<- $estimate-with (.get-float op "E")
        $estimate? (|| $estimate-with (.get op "e"))
        $target-working-hours (.get-int op "h" 140)
        $suppress-summary? (.get op "s")
        $long? (.get op "l")
        $year (.get-ints op "y")
        $month (.get-ints op "m")
        $verbose? (.get op "v"))
    (if (.get op "t") (begin (show-table) (return true))
        (.get op "a") (<- $year nil $month nil)    ; target all.
        (&& (nil? $year) (nil? $month)) (<- $year (list (.year now)) $month (list (.month now)))
        (nil? $year) (<- $year (list (.year now))))
    (if (nil? args) (parse)
        (dolist (file args)
          (with-open ($in file :read) (parse))))))
