; recursion test

(%assign same? (%lambda (x y) (%samep x y))
         not (%lambda (x) (same? x false)))

(same? 1 2)

(%assign + (%lambda (x y) (%xint_add x y))
         = (%lambda (x y) (%xint_eq x y))
         < (%lambda (x y) (%xint_lt x y))
         <= (%lambda (x y) (%if (= x y) true (< x y)))
         -- (%lambda (x) (%xint_add x -1)))

(%assign tarai (%lambda (x y z)
   (%if (<= x y)
      y
      (tarai (tarai (-- x) y z)
             (tarai (-- y) z x)
             (tarai (-- z) x y)))))

:tarai
(tarai 12 6 0)    ; 12

(%assign fib (%lambda (x)
   (%if (<= x 2)
     1
     (fib (+ (fib (-- x)) (fib (-- (-- x))))))))

:fib
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
