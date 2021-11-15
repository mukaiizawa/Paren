; Math module.

(<- $math.pi 3.14159265358979323846
    $math.e 2.71828182845904523536)

(function mean (:rest args)
  ; Returns arithmetic mean (average) of data.
  (/ (apply + args) (len args)))

(function geometric-mean (:rest args)
  ; Returns geometric mean of data.
  (exp (apply mean (map log args))))

(function harmonic-mean(:rest args)
  ; Returns Harmonic mean of data.
  (/ (len args) (apply + (map / args))))

(function median(:rest args)
  ; Returns Median (middle value) of data.
  (let (n (len args) n/2 (// n 2))
    (if (= (% n 2) 1) ([] args n/2)
        (mean ([] args (-- n/2)) ([] args n/2)))))

(function! main (args)
  (let (~= (f (x y) (< (abs (- x y)) 1.0e-14)))
    (assert (~= (mean 1 2 3 4 4) 2.8))
    (assert (~= (mean -1.0 2.5 3.25 5.75) 2.625))
    (assert (~= (geometric-mean 4 4 4 4) 4))
    (assert (~= (harmonic-mean 40 60) 48))
    (assert (~= (median 1 3 5) 3))
    (assert (~= (median 1 3 5 7) 4))))
