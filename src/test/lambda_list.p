; lambda list test.

;; required parameter
((lambda (x) x) true)
((lambda (x y z) (cons x (cons y (cons z nil)))) 1 2 3)

;; optional parameter
((lambda (:opt x) x))
((lambda (:opt x) x) 1)
((lambda (:opt (x 1) (y 2)) (cons x (cons y nil))) :x)
((lambda (:opt (x 1) (y 2)) (cons x (cons y nil))) :x :y)

;; rest parameter
((lambda (:rest rest) rest) 1 2 3 4 5)

;; keyword parameter
((lambda (:key x y z) (cons x (cons y (cons z nil)))) :x 1 :y 2 :z 3)
((lambda (:key (x 1) (y 2) (z 3)) (cons x (cons y (cons z nil)))) :x 1 :y -2 :z 3)
((lambda (:key (x 1) (y 2) (z 3)) (cons x (cons y (cons z nil)))) :z 1 :y -2 :x 3)
