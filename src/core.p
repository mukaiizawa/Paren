; paren core library.

; list
(<- list (lambda (:rest args) args)
    caar (lambda (x) (car (car x)))
    cadr (lambda (x) (car (cdr x)))
    cdar (lambda (x) (cdr (car x)))
    cddr (lambda (x) (cdr (cdr x)))
    caaar (lambda (x) (car (caar x)))
    caadr (lambda (x) (car (cadr x)))
    cadar (lambda (x) (car (cdar x)))
    caddr (lambda (x) (car (cddr x)))
    cdaar (lambda (x) (cdr (caar x)))
    cdadr (lambda (x) (cdr (cadr x)))
    cddar (lambda (x) (cdr (cdar x)))
    cdddr (lambda (x) (cdr (cddr x)))
    caaaar (lambda (x) (car (caaar x)))
    caaadr (lambda (x) (car (caadr x)))
    caadar (lambda (x) (car (cadar x)))
    caaddr (lambda (x) (car (caddr x)))
    cadaar (lambda (x) (car (cdaar x)))
    cadadr (lambda (x) (car (cdadr x)))
    caddar (lambda (x) (car (cddar x)))
    cadddr (lambda (x) (car (cdddr x)))
    cdaaar (lambda (x) (cdr (caaar x)))
    cdaadr (lambda (x) (cdr (caadr x)))
    cdadar (lambda (x) (cdr (cadar x)))
    cdaddr (lambda (x) (cdr (caddr x)))
    cddaar (lambda (x) (cdr (cdaar x)))
    cddadr (lambda (x) (cdr (cdadr x)))
    cdddar (lambda (x) (cdr (cddar x)))
    cddddr (lambda (x) (cdr (cdddr x))))

(macro function (name args :rest body)
  (list <- name (cons lambda (cons args body))))

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
                       (true nil)))))
      (rec args))))

; (<- $$backquote-depth 0)
