; Newton's method for finding root.

(<- dx 0.000001)

(function deriv (g)
  ; Returns the derivative of the function g.
  (f (x) (/ (- (g (+ x dx)) (g x)) dx)))

(function newton-transform (g)
  (let (dg (deriv g))
    (f (x) (- x (/ (g x) (dg x))))))

(function fixed-point (g)
  (let (step (f (x)
               (let (next (g x))
                 (if (< (abs (- x next)) dx) next
                     (step next)))))
    (step 1.0)))

(function newtons-method (g)
  (fixed-point (newton-transform g)))

(function xsqrt (x)
  (newtons-method (f (y) (- (pow y 2) x))))

(function! main (args)
  (assert (= (format "%.5f" (xsqrt 2)) (format "%.5f" (sqrt 2))))
  (assert (= (format "%.5f" (xsqrt 3)) (format "%.5f" (sqrt 3))))
  (assert (= (format "%.5f" (xsqrt 4)) (format "%.5f" (sqrt 4)))))
