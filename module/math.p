; Math module.

(<- $math.pi 3.14159265358979323846
    $math.e 2.71828182845904523536)

(function math.sqr (x)
  ; Returns the square of x.
  (exp x 2))

(function math.sqrt (x)
  ; Returns the square root of x.
  (if (< x 0) (raise ArgumentError "expected positive integer")
      (= x 0) 0
      (let (y (math.max 1.0 x) result y)
        (<- y (/ (+ (/ x y) y) 2))
        (while (< y result)
          (<- result y
              y (/ (+ (/ x y) y) 2)))
        result)))

(function math.max (:rest args)
  ; Returns maximum value from argument.
  (reduce (f (x y) (if (> x y) x y)) args))

(function math.min (:rest args)
  ; Returns minimum value from argument.
  (reduce (f (x y) (if (< x y) x y)) args))

(function! main (args)
  (assert (= (math.max 1 2 3) 3))
  (assert (= (math.min 1 2 3) 1))
  (assert (= (math.sqr 2) 4))
  (assert (= (math.sqrt 4) 2))
  (assert (= (math.sqrt 0.04) 0.2)))
