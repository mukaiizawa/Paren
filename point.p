; Point class

(class Point ()
  ; 二次元直行座標系上の点を表す。
  x y)

(method Point .init (:key (x 0) (y 0))
  (&x self x)
  (&y self y)
  self)

(method Point .x ()
  ; x座標を返す。
  (&x self))

(method Point .y ()
  ; y座標を返す。
  (&y self))

(method Point .toString ()
  (concat "(" (number->string (&x self)) "," (number->string (&y self)) ")"))

(method Point .equal (p)
  (assert (is-a? p Point))
  (and (= (&x self) (&x p)) (= (&y self) (&y p))))

(method Point .add (p)
  (assert (is-a? p Point))
  (.init (.new Point)
         :x (+ (&x self) (&x p))
         :y (+ (&y self) (&y p))))

(function main ()
  (<- p (.init (.new Point) :x 3 :y 4))
  (assert (.equal p (.init (.new Point) :x 3 :y 4)))
  (assert (not (.equal p (.init (.new Point) :x 2 :y 4))))
  (assert (not (.equal p (.init (.new Point) :x 3 :y 5))))
  (assert (not (.equal p (.init (.new Point) :x 2 :y 5))))
  (assert (string= (.toString p) "(3,4)")))
