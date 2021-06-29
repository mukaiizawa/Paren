; radix sort.

(import :rand)

(function radix-sort (lis)
  (let (radix 2 n (int (++ (log radix (apply max lis)))) buckets (list lis nil))
    (dotimes (i n)
      (let (wk (list nil nil))
        (dolist (bucket buckets)
          (dolist (x bucket)
            (let (i-th-digit (& (>> x i) 1))
              ([] wk i-th-digit (cons x ([] wk i-th-digit))))))
        (<- buckets (map reverse! wk))))
    (apply concat buckets)))

(function! main (args)
  (assert (= (radix-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (radix-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (radix-sort '(5 4 1 2 3)) '(1 2 3 4 5)))
  (assert (= (radix-sort (rand.shuffle! (.. 100))) (.. 100))))
