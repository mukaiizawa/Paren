; labels/goto

(assert (= (unwind-protect (return 0) 1 2) 0))
(unwind-protect (<- a 0) (<- a 1))
(assert (= a 1))
