; paren core library.

; fundamental list processor
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

; fundamental macro
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

; basic predicate
(function not (x)
  (if x nil true))

(function nil? (x)
  (same? x nil))

(function type? (x k)
  (same? (type x) k))

(function cons? (x)
  !(atom? x))

(function list? (x)
  (or (nil? x) (cons? x)))

; list processor
(function identity (x) x)

(function last-cons (lis)
  (if (nil? lis)
    nil
    (let ((rec (lambda (lis) (if (cdr lis) (rec (cdr lis)) lis))))
      (rec lis))))

(function last (lis)
  (car (last-cons lis)))

(function nth (lis n)
  (cond ((nil? lis) nil)
        ((= n 0) (car lis))
        (:default (rec (cdr lis) (-- n)))))

(function len (lis)
  (let ((rec (lambda (lis n)
               (if (nil? lis) n (rec (cdr lis) (++ n))))))
    (rec lis 0)))

(macro add (lis x)
  (list cdr (list last-cons lis) (list cons x nil)))

(macro push (lis x)
  (list <- lis (list cons x lis)))

(macro pop (lis)
  (list <- lis (list cdr lis)))

(function ->list (x)
  (if (list? x) x (list x)))

(function map (args f)
  (if args (cons (f (car args)) (map (cdr args) f))))

(function reverse (lis)
  (let ((rec (lambda (lis acc)
               (if (nil? lis) acc (rec (cdr lis) (cons (car lis) acc))))))
    (rec lis nil)))

(function reduce (args f :key (identity nil identity?))
  (let ((rec (lambda (args)
               (if (nil? (cdr args)) (car args)
                 (rec (cons (f (car args) (cadr args)) (cddr args)))))))
    (rec (if identity? (cons identity args) args))))

(function find (lis e :key (test =) (key identity))
  (if (nil? lis)
    nil
    (if (test (key (car lis)) e)
      (car lis)
      (find (cdr lis) e :test test :key key))))

(function find-if (lis f :key (key identity))
  (if (nil? lis)
    nil
    (if (f (key (car lis)))
      (car lis)
      (find-if (cdr lis) f :key key))))

;; associative list
(function get (alis key :key (test =))
  (if (nil? alis)
    nil
    (let ((first (car alis))
          (rest (cdr alis)))
      (if (test key (car first))
        (cdr first)
        (get (cdr alis) key :test test)))))

(function put (alis key val :key (test =))
  (if (nil? alis)
    nil
    (let ((first (car alis))
          (rest (cdr alis)))
      (if (test key (car first))
        (cdr first val)
        (put (cdr alis) key val :test test)))))

(function has-key? (alis key :key (test =))
  (and alis
       (or (test key (car (car alis)))
           (has-key? (cdr alis) key :test test))))

; numeric
(macro inc (x :opt (y 1))
  (list <- x (list + x y)))

(macro dec (x :opt (y 1))
  (list <- x (list - x y)))

(function + (:rest args)
  (reduce args number_add :identity 0))

(function ++ (x)
  (number_add x 1))

(function - (:rest args)
  (reduce (map (cdr args) negated) number_add :identity (car args)))

(function -- (x)
  (number_add x -1))

(function * (:rest args)
  (reduce args number_multiply :identity 1))

(function negated (x)
  (* x -1))

(function < (x y)
  (number_lt x y))

(function > (x y)
  (< y x))

(function <= (x y)
  !(< y x))

(function >= (x y)
  !(< x y))

; (<- $$backquote-depth 0)

; should be implement
; (<- q '(r s))
; ``(q ,q ,,q ,@q ,,'q ,',q ',,q ,@,q ,,@q ,@,@q)
;
; (CONS 'Q
;       (CONS Q
;             (CONS (R S)
;                   (APPEND Q
;                           (CONS Q
;                                 (CONS '(R S) (CONS (LIST 'QUOTE (R S)) (APPEND (R S) (LIST* R S (APPEND R S))))))))))
