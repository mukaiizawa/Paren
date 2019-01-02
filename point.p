; Point class

(class Point ()
  "二次元直交座標クラス
  このクラスのオブジェクトはイミュータブルである。"
  :x :y)

(method Point .init (:key (x 0) (y 0))
  (.x self x)
  (.y self y)
  self)

(method Point .equal? (p)
  (precondition (is-a? p Point))
  (and (= (.x self) (.x p)) (= (.y self) (.y p))))
(assert (.equal? (.init (.new Point)) (.init (.new Point))))
(assert !(.equal? (.init (.new Point)) (.init (.new Point) :x 1 :y 2)))

(method Point .add (p)
  (precondition (is-a? p Point))
  (.init (.new Point)
         :x (+ (.x self) (.x p))
         :y (+ (.y self) (.y p))))
(assert (.equal? (.add (.init (.new Point) :x 1) (.init (.new Point) :y 1)) (.init (.new Point) :x 1 :y 1)))
