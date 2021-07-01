; shell sort.

(import :rand)

; Marcin Ciura's gap sequence.
(<- $gaps '(701 301 132 57 23 10 4 1))

(function shell-sort (seq)
  (dolist (gap $gaps)
    (for (i gap) (< i (len seq)) (i (++ i))
      (let (j i x ([] seq i))
        (while (>= j gap)
          (let (k (- j gap))
            (if (>= x ([] seq k)) (break)
                ([] seq j ([] seq k)))
            (<- j k)))
        ([] seq j x))))
  seq)

(function! main (args)
  (assert (= (shell-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (shell-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (shell-sort '(5 4 1 2 3)) '(1 2 3 4 5)))
  (assert (= (shell-sort (rand.shuffle! (.. 100))) (.. 100))))
