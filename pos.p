; paren object system

(macro class ())
(macro method ())

(class Object ())

(method Object .init ()
        self)

(method Object .equals ())

(class Point () x y) ; (<- Point (clone Object))


(method Point init (:key (x 0) (y 0))
        (.y (.x self x) y))
; (<- (.init Point) (function (self :key (x 0) (y 0)) (.y (.x self x) y)))

(method Point add (q)
        ((.init (clone Point)) :x (+ (.x self) (.x q))
                               :y (+ (.y self) (.y q))))
; (<- (.add Point) (function (self q) ((.init (clone Point)) :x (+ (.x self) (.x q)) :y (+ (.y self) (.y q)))))

(<- p ((.init (clone Point)) :x 1 :y 2)
    q ((.init (clone Point)) :x 3 :y 2))

(<- r ((.add p) q))
