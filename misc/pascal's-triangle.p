; Pascal's triangle.

(function pascal-x-y (x y)
  (if (|| (= y 0) (= x y)) 1
      (+ (pascal-x-y (-- x) (-- y))
         (pascal-x-y (-- x) y))))

(function pascal-line (x)
  (map (f (y) (pascal-x-y x y))
       (.. 0 (++ x))))

(function! main (n)
  (foreach write (map pascal-line (.. 0 10))))
