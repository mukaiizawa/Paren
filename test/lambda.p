; lambda

;; required parameter
(print (same? ((lambda (x) x) :x) :x))
(print (same? ((lambda (x y) x) :x :y) :x))
(print (same? ((lambda (x y) y) :x :y) :y))

;; optional parameter
(print (same? ((lambda (x :opt o) o) :x) nil))
(print (same? ((lambda (x :opt o p) p) :x :o) nil))
(print (same? ((lambda (x :opt o (p :p)) p) :x :o) :p))
(print (same? ((lambda (x :opt o (p :p p?)) p?) :x :o) nil))
(print (same? ((lambda (x :opt o (p :p p?)) p?) :x :o :p) true))

;; rest parameter
(print (same? ((lambda (x :rest r) r) :x) nil))
(print (same? (car ((lambda (x :rest r) r) :x :r1 :r2)) :r1))
(print (same? (car (cdr ((lambda (x :rest r) r) :x :r1 :r2))) :r2))

;; keyword parameter
(print (same? ((lambda (x :key k) k) :x) nil))
(print (same? ((lambda (x :key k) k) :x :k :k) :k))
(print (same? ((lambda (x :key (k :k)) k) :x) :k))
(print (same? ((lambda (x :key (k :k k?)) k?) :x) nil))
(print (same? ((lambda (x :key (k :k k?)) k?) :x :k :k) true))
(print (same? ((lambda (:key k1 k2) k1) :k2 :k2 :k1 :k1) :k1))
(print (same? ((lambda (:key k1 k2) k2) :k2 :k2 :k1 :k1) :k2))
