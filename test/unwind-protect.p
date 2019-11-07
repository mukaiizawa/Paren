; labels/goto

;; return
(assert (= (unwind-protect (return 0) 1 2) 0))
(unwind-protect (<- a 0) (<- a 1))
(assert (= a 1))

;; goto
(labels (unwind-protect (goto :x) (<- x 1))
        (<- x 2)
        :x
        (assert (= x 1)))
