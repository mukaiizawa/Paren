; gnome sort.

(import :rand)

(function gnome-sort (seq)
  (let (i 1)
    (while (< i (len seq))
      (if (<= ([] seq (-- i)) ([] seq i)) (<- i (++ i))
          (begin
            (swap! seq i (-- i))
            (if (> i 1) (<- i (-- i))))))
    seq))

(function! main (args)
  (assert (= (gnome-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (gnome-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (gnome-sort '(5 4 1 2 3)) '(1 2 3 4 5)))
  (assert (= (gnome-sort (rand.shuffle! (.. 100))) (.. 100))))
