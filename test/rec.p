; recursion

(<- <= (lambda (x y) (if (= x y) true (< x y)))
    - (lambda (x y) (+ x (* y -1))))

:fib
(<- fib (lambda (x)
   (if (<= x 2) 1
       (+ (fib (- x 1)) (fib (- x 2))))))
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)

:tarai
(<- tarai (lambda (x y z)
   (if (<= x y)
      y
      (tarai (tarai (-- x) y z)
             (tarai (-- y) z x)
             (tarai (-- z) x y)))))
; (tarai 6 3 0)    ; 6
; (tarai 10 5 0)    ; 10
; (tarai 12 6 0)    ; 12