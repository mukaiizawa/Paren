; labels/goto

(unwind-protect (return (print 0))
                (print 1)
                (print 2)
                (print (+ 1 2)))
