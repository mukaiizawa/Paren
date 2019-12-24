; Fibonacci number.

(function fib (x)
  (if (> x 1) (+ (fib (-- x)) (fib (- x 2)))
      1))

(print (map (.. 0 5) fib))
