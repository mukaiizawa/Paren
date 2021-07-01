; counting sort.

(import :rand)

(function counting-sort (seq)
  (let (min (apply min seq) max (apply max seq) buf (array (++ (- max min))))
    (dotimes (i (len buf))
      ([] buf i 0))
    (dolist (x seq)
      (let (i (- x min))
        ([] buf i (++ ([] buf i)))))
    (for (i min j 0) (<= i max) (i (++ i))
      (dotimes (_ ([] buf (- i min)))
        ([] seq j i)
        (<- j (++ j))))
    seq))

(function! main (args)
  (assert (= (counting-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (counting-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (counting-sort '(5 4 1 2 3)) '(1 2 3 4 5)))
  (assert (= (counting-sort (rand.shuffle! (.. 100))) (.. 100))))
