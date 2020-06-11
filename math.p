; math

(global-symbol Math.pi 3.14159265358979323846)

(function Math.max (:rest args)
  (reduce args (lambda (x y) (if (> x y) x y))))

(function Math.min (:rest args)
  (reduce args (lambda (x y) (if (< x y) x y))))

(builtin-function Math.ceiling (x)
  ; So-called ceiling function.
  (assert (= (Math.ceiling 1.1) 2))
  (assert (= (Math.ceiling 0) 0))
  (assert (= (Math.ceiling -1.1) -1)))

(builtin-function Math.floor (x)
  ; So-called floor function.
  (assert (= (Math.floor 1.1) 1))
  (assert (= (Math.floor 0) 0))
  (assert (= (Math.floor -1.1) -2)))

(builtin-function Math.truncate (x)
  ; Truncate specified number x.
  (assert (= (Math.truncate 1.1) 1))
  (assert (= (Math.truncate 0) 0))
  (assert (= (Math.truncate -1.1) -1)))

(function main ()
  (assert (= (Math.max 1 2 3) 3))
  (assert (= (Math.min 1 2 3) 1)))
