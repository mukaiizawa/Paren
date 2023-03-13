; factorization

(function factorize (n)
  (for (s 2 e (sqrt n)) (<= s e) (s (++ s))
    (if (zero? (% n s)) (return (str s " " (factorize (/ n s))))))
  n)

(function! main (args)
  (if (nil? args) (loop (println (factorize (read))))
      (println (factorize (int (car args))))))
