; Math module.

(<- $math.pi 3.14159265358979323846
    $math.e 2.71828182845904523536)

(function sqr (x)
  ; Returns the square of x.
  (exp x 2))

(function sqrt (x)
  ; Returns the square root of x.
  (if (< x 0) (error "required positive number " x)
      (= x 0) 0
      (let (y (max 1.0 x) result y)
        (<- y (/ (+ (/ x y) y) 2))
        (while (< y result)
          (<- result y
              y (/ (+ (/ x y) y) 2)))
        result)))

(function max (:rest args)
  ; Returns maximum value from argument.
  (reduce (f (x y) (if (> x y) x y)) args))

(function min (:rest args)
  ; Returns minimum value from argument.
  (reduce (f (x y) (if (< x y) x y)) args))

(function! main (args)
  (assert (= (max 1 2 3) 3))
  (assert (= (min 1 2 3) 1))
  (assert (= (sqr 2) 4))
  (assert (= (sqrt 4) 2))
  (assert (= (sqrt 0.04) 0.2)))