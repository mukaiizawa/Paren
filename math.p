; Math module.

(global-symbol Math.pi 3.14159265358979323846)

(function Math.sqr (x)
  ; Returns the square of x.
  (exp x 2))

(function Math.sqrt (x)
  ; Returns the square root of x.
  (if (< x 0) (error "illegal argument")
      (= x 0) 0
      (let (y (Math.max 1.0 x) result y)
        (<- y (/ (+ (/ x y) y) 2))
        (while (< y result)
          (<- result y
              y (/ (+ (/ x y) y) 2)))
        result)))

(function Math.max (:rest args)
  (reduce (lambda (x y) (if (> x y) x y)) args))

(function Math.min (:rest args)
  (reduce (lambda (x y) (if (< x y) x y)) args))

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

(function! main ()
  (assert (= (Math.max 1 2 3) 3))
  (assert (= (Math.min 1 2 3) 1))
  (assert (= (Math.sqr 2) 4))
  (assert (= (Math.sqrt 4) 2))
  (assert (= (Math.sqrt 0.04) 0.2)))
