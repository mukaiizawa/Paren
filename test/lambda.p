; lambda

;; required parameter
(assert (same? ((lambda (x) x) :x) :x))
(assert (same? ((lambda (x y) x) :x :y) :x))
(assert (same? ((lambda (x y) y) :x :y) :y))

;; optional parameter
(assert (same? ((lambda (x :opt o) o) :x) nil))
(assert (same? ((lambda (x :opt o p) p) :x :o) nil))
(assert (same? ((lambda (x :opt o (p :p)) p) :x :o) :p))
(assert (same? ((lambda (x :opt o (p :p p?)) p?) :x :o) nil))
(assert (same? ((lambda (x :opt o (p :p p?)) p?) :x :o :p) true))

;; rest parameter
(assert (same? ((lambda (x :rest r) r) :x) nil))
(assert (same? (car ((lambda (x :rest r) r) :x :r1 :r2)) :r1))
(assert (same? (car (cdr ((lambda (x :rest r) r) :x :r1 :r2))) :r2))

;; keyword parameter
(assert (same? ((lambda (x :key k) k) :x) nil))
(assert (same? ((lambda (x :key k) k) :x :k :k) :k))
(assert (same? ((lambda (x :key (k :k)) k) :x) :k))
(assert (same? ((lambda (x :key (k :k k?)) k?) :x) nil))
(assert (same? ((lambda (x :key (k :k k?)) k?) :x :k :k) true))
(assert (same? ((lambda (:key k1 k2) k1) :k2 :k2 :k1 :k1) :k1))
(assert (same? ((lambda (:key k1 k2) k2) :k2 :k2 :k1 :k1) :k2))
