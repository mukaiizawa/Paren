; bubble sort.

(import :rand)

(function bubble-sort (seq)
  (let (sorted? nil)
    (while (! sorted?)
      (<- sorted? true)
      (dotimes (i (-- (len seq)))
        (if (< ([] seq (++ i)) ([] seq i))
            (begin
              (<- sorted? nil)
              (swap! seq i (++ i))))))
    seq))

(function! main (args)
  (assert (= (bubble-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (bubble-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (bubble-sort '(5 4 1 2 3)) '(1 2 3 4 5)))
  (assert (= (bubble-sort (rand.shuffle! (.. 100))) (.. 100))))
