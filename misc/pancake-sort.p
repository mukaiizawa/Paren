; pancake sort.

(import :rand)

(function flip (seq i)
  (concat (reverse! (slice seq 0 (++ i)))
          (slice seq (++ i))))

(function pancake-sort (seq)
  (let (n (len seq))
    (while (> n 1)
      (let (max-val (apply max (slice seq 0 n))
                    max-pos (position (f (x) (= x max-val)) seq))
        (if (!= max-pos n) (<- seq (flip (flip seq max-pos) (-- n))))
        (<- n (-- n))))
    seq))

(function! main (args)
  (assert (= (pancake-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (pancake-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (pancake-sort '(5 4 1 2 3)) '(1 2 3 4 5)))
  (assert (= (pancake-sort (rand.shuffle! (.. 100))) (.. 100))))
