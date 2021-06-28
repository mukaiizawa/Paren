; heap sort.

(function heap-sort (seq)
  (let (n (len seq))
    (for (s (// (- n 2) 2)) (>= s 0) (s (-- s))
      (make-heap seq s n))
    (for (e (-- n)) (> e 0) (e (-- e))
      (swap! seq 0 e)
      (make-heap seq 0 e))
    seq))

(function make-heap (seq s e)
  ;; Makes the specified range (s <= i < e) of the sequence a heap.
  ;;     (1 2 3 4 5) 1 5
  ;;     =>  (1 5 3 4 2) 0 5
  ;;         =>  (5 4 3 1 2)
  (let (parent s child nil)
    (while (< (<- child (++ (* parent 2))) e)
      (if (&& (< (++ child) e) (< ([] seq child) ([] seq (++ child)))) (<- child (++ child)))
      (if (> ([] seq parent) ([] seq child)) (break))    ; already heap.
      (swap! seq parent child)
      (<- parent child))))

(function! main (args)
  (assert (= (heap-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (heap-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (heap-sort '(5 4 1 2 3)) '(1 2 3 4 5))))
