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

(assert (and))
(assert (same? (and :x :y :z) :z))
(assert (same? (and :x nil :z) nil))
