; point module.

(class Point ()
  ; Represents a point in a two-dimensional rectangular coordinate system.
  ; Create an instance with Point.of function.
  x y)

(function Point.of (x y)
  ; Returns a point instance corresponding to the coordinates (x, y).
  (if (! (every? number? (list x y))) (.raise self "illegal arguments."))
  (let (p (.new Point))
    (&x! p x)
    (&y! p y)))

(method Point .x ()
  ; Returns x coordinate of the receiver.
  (&x self))

(method Point .y ()
  ; Returns y coordinate of the receiver.
  (&y self))

(method Point .to-s ()
  (string "(" (&x self) ", " (&y self) ")"))

(method Point .eq (p)
  (&& (is-a? p Point)
      (= (&x self) (&x p)) (= (&y self) (&y p))))

(method Point .add (p)
  ; Returns the instance of Point corresponds to the sum of receiver and p.
  (if (is-a? p Point) (Point.of (+ (&x self) (&x p)) (+ (&y self) (&y p)))
      (.raise self "illegal arguments." p)))

(function! main (args)
  (let (p (Point.of 3 4))
    (assert (.eq p (Point.of 3 4)))
    (assert (! (.eq p (Point.of 2 4))))
    (assert (! (.eq p (Point.of 3 5))))
    (assert (! (.eq p (Point.of 2 5))))
    (assert (.eq p (.add (Point.of 1 1) (Point.of 2 3))))
    (assert (memeq? (.to-s p) "(3, 4)"))))
