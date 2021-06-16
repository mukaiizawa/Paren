; Collatz conjecture.

(function step (n)
  (write n)
  (if (= (% n 2) 0) (step (/ n 2))
      (!= n 1) (step (++ (* 3 n)))))

(function! main (args)
  (let (n (int (car args)))
    (if (<= n 0) (raise ArgumentError)
        (step n))))
