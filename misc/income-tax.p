; income tax simulation.

; income-s income-e tax-rate deduction
;; see https://www.nta.go.jp/taxes/shiraberu/taxanswer/shotoku/2260.htm
(<- $tax-table
    (group '(1000 1949000 0.05 0
             1950000 3299000 0.10 97500
             3300000 6949000 0.20 427500
             6950000 8999000 0.23 636000
             9000000 17999000 0.33 1536000
             18000000 39999000 0.40 2796000
             40000000 nil 0.45 4796000) 4)
    $income-upper 20000000
    $income-step 100000)

(function find-tax-record (table income)
  (let (record (car table) (_ income-e _ _) record)
    (if (&& income-e (> income income-e)) (find-tax-record (cdr table) income)
        record)))

(function list->tsv (lis)
  (join (map str lis) "\t"))

(function income->tsv (income)
  (let ((_ _ tax-rate deduction) (find-tax-record $tax-table income)
                                 tax (- (* income tax-rate) deduction))
    (list->tsv (list income tax (- income tax)))))

(function! main (args)
  (foreach write-line (map income->tsv (.. 0 $income-upper $income-step))))
