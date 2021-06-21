; quick sort.

(function quick-sort (lis)
  (if (nil? lis) nil
      (let (pivot (car lis) rest (cdr lis))
        (concat (quick-sort (select (f (x) (< x pivot)) rest))
                (list pivot)
                (quick-sort (select (f (x) (>= x pivot)) rest))))))

(function! main (args)
  (assert (= (quick-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (quick-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (quick-sort '(5 4 1 2 3)) '(1 2 3 4 5))))
