; point module.

(class Point ()
  ; Represents a point in a two-dimensional rectangular coordinate system.
  ; Create an instance with point function.
  x y)

(function point (x y)
  ; Returns a point instance corresponding to the coordinates (x, y).
  (if (! (every? number? (list x y))) (raise ArgumentError))
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
  (str "(" (&x self) ", " (&y self) ")"))

(method Point .eq (p)
  (&& (is-a? p Point)
      (= (&x self) (&x p)) (= (&y self) (&y p))))

(method Point .add (p)
  ; Returns the instance of Point corresponds to the sum of receiver and p.
  (if (is-a? p Point) (point (+ (&x self) (&x p)) (+ (&y self) (&y p)))
      (raise ArgumentError "expected instance of Point")))

(function! main (args)
  (let (p (point 3 4))
    (assert (.eq p (point 3 4)))
    (assert (! (.eq p (point 2 4))))
    (assert (! (.eq p (point 3 5))))
    (assert (! (.eq p (point 2 5))))
    (assert (.eq p (.add (point 1 1) (point 2 3))))
    (assert (= (.to-s p) "(3, 4)"))))
