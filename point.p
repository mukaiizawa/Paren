; point module.

(class Point ()
  ; Represents a point in a two-dimensional rectangular coordinate system.
  ; Create an instance with Point.of function.
  x y)

(function Point.of (x y)
  (&y<- (&x<- (.new Point) x) y))

(method Point .x ()
  ; Returns x coordinate.
  (&x self))

(method Point .y ()
  ; Returns y coordinate.
  (&y self))

(method Point .to-s ()
  (string "(" (&x self) ", " (&y self) ")"))

(method Point .eq (p)
  (&& (is-a? p Point)
      (= (&x self) (&x p)) (= (&y self) (&y p))))

(method Point .add (p)
  (Point.of (+ (&x self) (&x p)) (+ (&y self) (&y p))))

(function! main ()
  (let (p (Point.of 3 4))
    (assert (.eq p (Point.of 3 4)))
    (assert (! (.eq p (Point.of 2 4))))
    (assert (! (.eq p (Point.of 3 5))))
    (assert (! (.eq p (Point.of 2 5))))
    (assert (string= (.to-s p) "(3, 4)"))))
