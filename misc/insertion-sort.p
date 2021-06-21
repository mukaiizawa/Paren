; insertion sort.

(function insert (x lst)
  (if (nil? lst) (list x)
      (let (y (car lst))
        (if (<= x y) (cons x lst)
            (cons y (insert x (cdr lst)))))))
 
(function insertion-sort (lst)
  (if (nil? lst) nil
      (insert (car lst) (insertion-sort (cdr lst)))))

(function! main (args)
  (assert (= (insertion-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (insertion-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (insertion-sort '(5 4 1 2 3)) '(1 2 3 4 5))))
