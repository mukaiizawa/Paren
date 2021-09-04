; bogo sort.

(import :rand)

(function bogo-sort (lis)
  (if (apply <= lis) lis
      (bogo-sort (rand.shuffle! lis))))

(function! main (args)
  (assert (= (bogo-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (bogo-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (bogo-sort '(5 4 1 2 3)) '(1 2 3 4 5)))
  (assert (= (bogo-sort (rand.shuffle! (.. 5))) (.. 5))))
