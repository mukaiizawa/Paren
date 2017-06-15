#|
  paren core routine.
|#

(def caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar
  caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr
  cddaar cddadr cdddar)
(<- caar (fn x (car (car x)))
    cadr (fn x (car (cdr x)))
    cdar (fn x (cdr (car x)))
    cddr (fn x (cdr (cdr x)))
    caaar (fn x (car (car (car x))))
    caadr (fn x (car (car (cdr x))))
    cadar (fn x (car (cdr (car x))))
    caddr (fn x (car (cdr (cdr x))))
    cdaar (fn x (cdr (car (car x))))
    cdadr (fn x (cdr (car (cdr x))))
    cddar (fn x (cdr (cdr (car x))))
    cdddr (fn x (cdr (cdr (cdr x))))
    caaaar (fn x (car (car (car (car x)))))
    caaadr (fn x (car (car (car (cdr x)))))
    caadar (fn x (car (car (cdr (car x)))))
    caaddr (fn x (car (car (cdr (cdr x)))))
    cadaar (fn x (car (cdr (car (car x)))))
    cadadr (fn x (car (cdr (car (cdr x)))))
    caddar (fn x (car (cdr (cdr (car x)))))
    cadddr (fn x (car (cdr (cdr (cdr x)))))
    cdaaar (fn x (cdr (car (car (car x)))))
    cdaadr (fn x (cdr (car (car (cdr x)))))
    cdadar (fn x (cdr (car (cdr (car x)))))
    cdaddr (fn x (cdr (car (cdr (cdr x)))))
    cddaar (fn x (cdr (cdr (car (car x)))))
    cddadr (fn x (cdr (cdr (car (cdr x)))))
    cdddar (fn x (cdr (cdr (cdr (car x)))))
    cddddr (fn x (cdr (cdr (cdr (cdr x))))))

(def list)
(<- list (fn lis lis))

(def a)
(<- a (fn (a b) a))
