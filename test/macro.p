; macro

(macro and (:rest args)
  (if (same? args nil) true
      (let (rec (lambda (l)
                  (if (cdr l)
                          (cons if (cons (car l)
                                         (cons (rec (cdr l))
                                               nil)))
                      (car l))))
        (rec args))))

(print (and))
(print (same? (and :x :y :z) :z))
(print (same? (and :x nil :z) nil))
