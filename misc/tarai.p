; tarai function.

(function tarai (x y z)
  (if (<= x y) y
      (tarai (tarai (-- x) y z)
             (tarai (-- y) z x)
             (tarai (-- z) x y))))

(function! main (args)
  (timeit (tarai 12 6 0)))
