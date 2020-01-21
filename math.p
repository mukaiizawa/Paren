; math

(function max (:rest args)
  (reduce args (lambda (x y) (if (> x y) x y))))

(function min (:rest args)
  (reduce args (lambda (x y) (if (< x y) x y))))

(builtin-function ceiling (x)
  ; So-called ceiling function.
  (assert (= (ceiling 1.1) 2))
  (assert (= (ceiling 0) 0))
  (assert (= (ceiling -1.1) -1)))

(builtin-function floor (x)
  ; So-called floor function.
  (assert (= (floor 1.1) 1))
  (assert (= (floor 0) 0))
  (assert (= (floor -1.1) -2)))

(builtin-function truncate (x)
  ; Truncate specified number x.
  (assert (= (truncate 1.1) 1))
  (assert (= (truncate 0) 0))
  (assert (= (truncate -1.1) -1)))

(function main ()
  (assert (= (max 1 2 3) 3))
  (assert (= (min 1 2 3) 1)))
