; paren core library.


; list
(<- list (lambda (:rest args) args))

(macro function (name args :rest body)
  (list <- name (cons lambda (cons args body))))

(function caar (x) (car (car x)))
(function cadr (x) (car (cdr x)))
(function cdar (x) (cdr (car x)))
(function cddr (x) (cdr (cdr x)))
(function caaar (x) (car (caar x)))
(function caadr (x) (car (cadr x)))
(function cadar (x) (car (cdar x)))
(function caddr (x) (car (cddr x)))
(function cdaar (x) (cdr (caar x)))
(function cdadr (x) (cdr (cadr x)))
(function cddar (x) (cdr (cdar x)))
(function cdddr (x) (cdr (cddr x)))
(function caaaar (x) (car (caaar x)))
(function caaadr (x) (car (caadr x)))
(function caadar (x) (car (cadar x)))
(function caaddr (x) (car (caddr x)))
(function cadaar (x) (car (cdaar x)))
(function cadadr (x) (car (cdadr x)))
(function caddar (x) (car (cddar x)))
(function cadddr (x) (car (cdddr x)))
(function cdaaar (x) (cdr (caaar x)))
(function cdaadr (x) (cdr (caadr x)))
(function cdadar (x) (cdr (cadar x)))
(function cdaddr (x) (cdr (caddr x)))
(function cddaar (x) (cdr (cdaar x)))
(function cddadr (x) (cdr (cdadr x)))
(function cdddar (x) (cdr (cddar x)))
(function cddddr (x) (cdr (cdddr x)))

(macro cond (:rest expr)
  (if (nil? expr)
    nil
    (list if (caar expr) (cons begin (cdar expr)) (cons cond (cdr expr)))))

(macro begin-if (test :rest body)
  (list if test (cons begin body)))

(macro or (:rest expr)
  (if expr (list if (car expr) true (cons or (cdr expr)))))

(macro and (:rest expr)
  (if (nil? (cdr expr))
    (car expr)
    (list if (car expr) (cons and (cdr expr)))))

(function nil? (x)
  (same? x nil))

(function not (x)
  (if x nil true))

(function map (args f)
  (if args (cons (f (car args)) (map (cdr args) f))))

(function reverse (lis)
  (let ((rec (lambda (lis acc)
               (if (nil? lis) acc (rec (cdr lis) (cons (car lis) acc))))))
    (rec lis nil)))

(function cons? (x)
  (not (atom? x)))

(function list? (x)
  (or (nil? x) (cons? x)))

(function ->list (x)
  (if (list? x) x (list x)))

(function reduce (args f :key (identity nil identity?))
  (let ((rec (lambda (args)
               (if (nil? (cdr args)) (car args)
                 (rec (cons (f (car args) (cadr args)) (cddr args)))))))
    (rec (if identity? (cons identity args) args))))

; number
(function + (:rest args)
  (reduce args number_add :identity 0))

(function * (:rest args)
  (reduce args number_multiply :identity 1))

(function negated (x)
  (* x -1))

(function - (:rest args)
  (reduce (map (cdr args) negated) number_add :identity (car args)))

(function = (:rest args)
  (if (nil? (cdr args))
    nil
    (let ((rec (lambda (args)
                 (cond ((nil? (cdr args)) (car args))
                       ((number_eq (car args) (cadr args)) (rec (cdr args)))
                       (:default nil)))))
      (rec args))))


; (<- $$backquote-depth 0)
