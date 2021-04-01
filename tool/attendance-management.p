; attendance management.

; # input specification
; Input must be sorted
;
; Ignore blank lines or lines starting with `#`;
;
;     # date,start time,end time,deduction time
;     2021-04-01,8:00,18:00,1
;     2021-04-02,8:00,18:00,1
;     ...

(function ->yyyy-mm (yyyy-mm-dd)
  (submem yyyy-mm-dd 0 7))

(function group-by-month (records)
  ;; (("2021-04-01" nil)
  ;;  ("2021-04-06" 420)
  ;;  ...
  ;;  ("2021-07-01" 480))
  ;; ->
  ;; (("2021-04" ("2021-04-01" nil) ("2021-04-06" 420) ...)
  ;;  ...
  ;;  ("2021-07" ("2021-07-01" 420))
  (let (yyyy-mm (->yyyy-mm (caar records)) months nil result nil)
    (dolist (record records)
      (let ((yyyy-mm-dd working-time) record)
        (if (memprefix? yyyy-mm-dd yyyy-mm) (push! record months)
            (begin
              (push! (cons yyyy-mm months) result)
              (<- yyyy-mm (->yyyy-mm yyyy-mm-dd)
                  months (list record))))))
    (push! (cons yyyy-mm months) result)
    (reverse! result)))

(function parse-line (line)
  ;; yyyy-mm-dd,mm:dd,mm:dd,deduction-time
  ;; -> yyyy-mm-dd,working-time
  (let ((yyyy-mm-dd start end deduction-time) (split line ",")
                                              hhmm->min (f (hhmm)
                                                          (let ((h m) (map str->num (split hhmm ":")))
                                                            (+ (* 60 h) m))))
    (list yyyy-mm-dd (if (!= start "") (- (hhmm->min end)
                                          (hhmm->min start)
                                          (if (memempty? deduction-time) 0
                                              (* 60 (str->num deduction-time))))))))

(function parse-month (tree)
  ;; ("2021-04" ("2021-04-01" nil) ("2021-04-06" 420) ...)
  (let ((yyyy-mm :rest attendances) tree
                                    active-attendances (except (f (x) (nil? (cadr x))) attendances)
                                    working-days (len active-attendances)
                                    working-hours (/ (apply + (map cadr active-attendances)) 60)
                                    average-working-hours (if (= working-days 0) 'NaN
                                                              (/ working-hours working-days)))
    (join (map str (list yyyy-mm
                         working-days
                         working-hours
                         average-working-hours))
          ",")))

(function! main (args)
  (write-line "# year-month,working days,working hours,average working hours")
  (foreach write-line
           (map parse-month
                (group-by-month 
                  (map parse-line
                       (except (f (line) (|| (memempty? line) (memprefix? line "#")))
                               (collect read-line)))))))
