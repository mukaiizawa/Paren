; income tax simulation.

; range-s range-e tax-rate deduction
;; see https://www.nta.go.jp/taxes/shiraberu/taxanswer/shotoku/2260.htm
(<- $tax-table
    (group '(1000 1949000 0.05 0
             1950000 3299000 0.10 97500
             3300000 6949000 0.20 427500
             6950000 8999000 0.23 636000
             9000000 17999000 0.33 1536000
             18000000 39999000 0.40 2796000
             40000000 nil 0.45 4796000) 4)
    $income-upper 40000000
    $income-step 100000)

(function find-tax-record (table x)
  (let (record (car table)
               (range-s range-e tax-rate deduction) record)
    (if (|| (nil? range-e) (<= x range-e)) record
        (find-tax-record (cdr table) x))))

(function! main (args)
  (foreach (f (x)
             (let (tax nil (range-s range-e tax-rate deduction) (find-tax-record $tax-table x))
               (write-line (join (map string
                                      ; income	tax	rest
                                      (list x (<- tax (- (* x tax-rate) deduction)) (- x tax)))
                                 "\t" ))))
           (.. 0 $income-upper $income-step)))
