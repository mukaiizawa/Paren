; point module.

(class Point ()
  ; Represents a point in a two-dimensional rectangular coordinate system.
  x y)

(function point (x y)
  (.init (.new Point) :x x :y y))

(method Point .init (:key (x 0) (y 0))
  ; Initialize by specifying x and y coordinates.
  (&y! (&x! self x) y))

(method Point .x ()
  ; Returns x coordinate.
  (&x self))

(method Point .y ()
  ; Returns y coordinate.
  (&y self))

(method Point .to-s ()
  (string "(" (&x self) "," (&y self) ")"))

(method Point .eq (p)
  (and (is-a? p Point) (= (&x self) (&x p)) (= (&y self) (&y p))))

(method Point .add (p)
  (.init (.new Point)
         :x (+ (&x self) (&x p))
         :y (+ (&y self) (&y p))))

(function! main ()
  (with (p (point 3 4))
    (assert (.eq p (point 3 4)))
    (assert (not (.eq p (point 2 4))))
    (assert (not (.eq p (point 3 5))))
    (assert (not (.eq p (point 2 5))))
    (assert (seqeq? (.to-s p) "(3,4)"))))
