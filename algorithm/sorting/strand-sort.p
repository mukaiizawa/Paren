; strand sort.

(import :rand)

(function merge (l1 l2)
  (if (nil? l1) l2
      (nil? l2) l1
      (< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2))
      (cons (car l2) (merge l1 (cdr l2)))))

(function strand-sort (lis :opt sorted)
  (if (nil? lis) sorted
      (let (l1 nil l2 (list (car lis)))
        (while (<- lis (cdr lis))
          (let (x (car lis))
            (if (< x (car l2)) (push! x l1)
                (push! x l2))))
        (strand-sort (reverse! l1) (merge (reverse! l2) sorted)))))

(function! main (args)
  (assert (= (strand-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (strand-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (strand-sort '(5 4 1 2 3)) '(1 2 3 4 5)))
  (assert (= (strand-sort (rand.shuffle! (.. 100))) (.. 100))))
