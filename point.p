; point

(class Point ()
  ; Represents a point in a two-dimensional rectangular coordinate system.
  x y)

(method Point .init (:key (x 0) (y 0))
  ; Initialize by specifying x and y coordinates.
  (&x self x)
  (&y self y)
  self)

(method Point .x ()
  ; Returns x coordinate.
  (&x self))

(method Point .y ()
  ; Returns y coordinate.
  (&y self))

(method Point .toString ()
  (string "(" (&x self) "," (&y self) ")"))

(method Point .equal (p)
  (and (is-a? p Point) (= (&x self) (&x p)) (= (&y self) (&y p))))

(method Point .add (p)
  (.init (.new Point)
         :x (+ (&x self) (&x p))
         :y (+ (&y self) (&y p))))

(function main ()
  (let (p (.init (.new Point) :x 3 :y 4))
    (assert (.equal p (.init (.new Point) :x 3 :y 4)))
    (assert (not (.equal p (.init (.new Point) :x 2 :y 4))))
    (assert (not (.equal p (.init (.new Point) :x 3 :y 5))))
    (assert (not (.equal p (.init (.new Point) :x 2 :y 5))))
    (assert (string= (.toString p) "(3,4)"))))
