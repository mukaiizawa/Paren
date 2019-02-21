; macro

(<- list (lambda (:rest args) args)
    same? (lambda (x) (samep x))
    null (lambda (x) (samep x nil)))

(macro progn (:rest body)
  (list (cons lambda (cons nil body))))

(progn 1 2 3)

(progn (<- a 5) 2 a)
