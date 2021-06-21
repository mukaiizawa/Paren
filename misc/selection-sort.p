; selection sort.

(function min-index (seq)
  (let (index 0)
    (dotimes (i (-- (len seq)))
      (if (< ([] seq (++ i)) ([] seq index)) (<- index (++ i))))
    index))

(function selection-sort (seq)
  (if (> (len seq) 1)
      (dotimes (i (-- (len seq)))
        (swap! seq i (+ i (min-index (slice seq i))))))
  seq)

(function! main (args)
  (assert (= (selection-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (selection-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (selection-sort '(5 4 1 2 3)) '(1 2 3 4 5))))
