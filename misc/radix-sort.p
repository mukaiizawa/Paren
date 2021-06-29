; radix sort.

(import :rand)

(function radix-sort (lis)
  (let (radix 2 n (int (++ (log radix (apply max lis)))) buckets (array (list lis nil)))
    (dotimes (i n)
      (let (new-buckets (array radix))
        (dotimes (j radix)
          (dolist (x ([] buckets j))
            (let (i-th-digit (& (>> x i) 1))
              ([] new-buckets i-th-digit (cons x ([] new-buckets i-th-digit))))))
        (<- buckets (array (map reverse! (array->list new-buckets))))))
    (apply concat (array->list buckets))))

(function! main (args)
  (assert (= (radix-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (radix-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (radix-sort '(5 4 1 2 3)) '(1 2 3 4 5)))
  (assert (= (radix-sort (rand.shuffle! (.. 100))) (.. 100))))
