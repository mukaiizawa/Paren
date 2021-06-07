; Greatest common divisor.

(function gcd (n m)
  ;; tail recursion.
  (if (= n m) n
      (> n m) (gcd (- n m) m)
      (gcd (- m n) n)))

(function! main (args)
  (timeit (write (gcd 540001 2))))
