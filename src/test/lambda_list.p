; lambda list test.

(<- list (lambda (:rest x) x))

;; required parameter
((lambda (x) x) true)
((lambda (x y z) (list x y z)) 1 2 3)

;; optional parameter
((lambda (:opt x) (list x)))
((lambda (:opt x (y 2)) (list x)) 1)
((lambda (:opt (x 1) (y 2)) (list x y)) :x)
((lambda (:opt (x 1) (y 2)) (list x y)) :x :y)

;; rest parameter
((lambda (:rest rest) rest) 1 2 3 4 5)

;; keyword parameter
((lambda (:key x y z) (list x y z)) :x 1 :y 2 :z 3)
((lambda (:key (x 1) (y 2) (z 3)) (list x y z)) :x 1 :y -2 :z 3)
((lambda (:key (x 1) (y 2) (z 3)) (list x y z)) :z 1 :y -2 :x 3)
((lambda (:key (x 1 x?) (y 2 y?) (z 3 z?)) (list x x? y y? z z?)) :z 1 :y -2 :x 3)
((lambda (:key (x 1 x?) (y 2 y?) (z 3 z?)) (list x? y? z?)) :z 1)
