; merge sort.

(function merge (left right)
  (if (nil? left) right
      (nil? right) left
      (< (car left) (car right)) (cons (car left) (merge (cdr left) right))
      (cons (car right) (merge left (cdr right)))))

(function merge-sort (lis)
  (let (i (// (len lis) 2))
    (if (= i 0) lis
        (merge (merge-sort (slice lis 0 i))
               (merge-sort (slice lis i))))))

(function! main (args)
  (assert (= (merge-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (merge-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (merge-sort '(5 4 1 2 3)) '(1 2 3 4 5))))
