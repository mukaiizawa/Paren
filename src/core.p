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

; fundamental operator
(macro function (name args :rest body)
  (list <- name (cons lambda (cons args body))))

(macro cond (:rest expr)
  (if (nil? expr) nil
    (list if (caar expr) (cons begin (cdar expr)) (cons cond (cdr expr)))))

(macro begin-if (test :rest body)
  (list if test (cons begin body)))

(macro or (:rest expr)
  (if expr (list if (car expr) (car expr) (cons or (cdr expr)))))

(macro and (:rest expr)
  (if (nil? (cdr expr)) (car expr)
    (list if (car expr) (cons and (cdr expr)))))

(macro assert (test)
  (list begin-if (list not test)
        (list print (list list :AssertionFailed (list quote test)))
        '(quit)))

(function error (:rest args)
  (print (cons :Error args))
  (quit))

; basic predicate
(function not (x)
  (if x nil true))

(function /= (x y)
  !(= x y))

(function nil? (x)
  (same? x nil))

(function type? (x k)
  (same? (type x) k))

(function cons? (x)
  !(atom? x))

(function list? (x)
  (or (nil? x) (cons? x)))

(function all-satisfy? (lis f)
  (if (nil? lis) true
    (and (f (car lis)) (all-satisfy? (cdr lis) f))))

(function any-satisfy? (lis f)
  (if lis (or (f (car lis)) (any-satisfy? (cdr lis) f))))

(function each-pair-satisfy? (lis f)
  (if (nil? (cdr lis)) true
    (and (f (car lis) (cadr lis)) (each-pair-satisfy? (cdr lis) f))))

; list processor
(function identity (x) x)

(function sublist (lis s :opt e)
  (let ((len (length lis))
        (e (or e len))
        (rec (lambda (lis n)
               (if (= n 0) nil
                 (cons (car lis) (rec (cdr lis) (-- n)))))))
    (if (or (> s e) (< s 0) (> e len)) :error)
    (rec (nthcdr lis s) (- e s))))

(function copy-list (lis)
  (sublist lis 0 (length lis)))

(function last-cons (lis)
  (if (nil? lis) nil
    (let ((rec (lambda (lis) (if (cdr lis) (rec (cdr lis)) lis))))
      (rec lis))))

(function last (lis)
  (car (last-cons lis)))

(function nthcdr (lis n)
  (cond ((nil? lis) nil)
        ((<= n 0) lis)
        (:default (nthcdr (cdr lis) (-- n)))))

(function nth (lis n)
  (car (nthcdr lis n)))

(function length (lis)
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
  (if (nil? lis) nil
    (if (test (key (car lis)) e) (car lis)
      (find (cdr lis) e :test test :key key))))

(function find-if (lis f :key (key identity))
  (if (nil? lis) nil
    (if (f (key (car lis))) (car lis)
      (find-if (cdr lis) f :key key))))

; numeric
(macro inc (x :opt (y 1))
  (list <- x (list + x y)))

(macro dec (x :opt (y 1))
  (list <- x (list - x y)))

(function ++ (x)
  (+ x 1))

(function - (:rest args)
  (reduce (map (cdr args) negated) + :identity (car args)))

(function -- (x)
  (+ x -1))

(function negated (x)
  (* x -1))

(function > (:rest args)
  (each-pair-satisfy? args (lambda (x y) (< y x))))

(function <= (:rest args)
  (each-pair-satisfy? args (lambda (x y) !(< y x))))

(function >= (:rest args)
  (each-pair-satisfy? args (lambda (x y) !(< x y))))

; TODO
; (function even? (x))
; (function odd? (x))

(macro unquote (:rest forms)
  (error :comma-not-inside-backquote))

(macro splice (:rest forms)
  (unquote))

; quote n-times
(macro nquote (expr n)
  (let ((rec (lambda (expr n)
               (if (<= n 0) expr
                 (list quote (rec expr (-- n)))))))
    (rec expr n)))

(macro backquote (expr)
  (let ((level 0)
        (nquote (macro (expr l)
                  (if (= l 0) expr
                    (nquote expr (-- l)))))
        (traverse (lambda (tree)
                    (map tree (lambda (x) (list quote x))))))
    (if (atom? expr) (list quote expr)
      (nquote expr level))))

; pos
; {{{
; (<- Object '((:super nil)
;              (:type :Object)))
; (function . (object property :opt (val nil val?))
;   (let ((pair (find object property :key car)))
;     (if (nil? pair) (assert (list :NotFountProperty property)))
;     (if val? (cdr pair val) (cdr pair))))
; (print (. Object :type)) ; :Object
; (print (. Object :type)) ; :Object
; }}}

; test
; {{{

;;; cxr
(assert (= (list 1 2 3) '(1 2 3)))
(assert (same? (caar '((true))) true))
(assert (same? (cadr '(x true)) true))
(assert (= (cdar '((x true))) '(true)))
(assert (= (cddr '(x y true)) '(true)))
(assert (same? (caaar '(((true)))) true))
(assert (same? (caadr '(x (true))) true))
(assert (same? (cadar '((x true))) true))
(assert (same? (caddr '(x y true)) true))
(assert (= (cdaar '(((x true)))) '(true)))
(assert (= (cdadr '(x (y true))) '(true)))
(assert (= (cddar '((x y true))) '(true)))
(assert (= (cdddr '(x y z true)) '(true)))
(assert (same? (caaaar '((((true))))) true))
(assert (same? (caaadr '(x ((true)))) true))
(assert (same? (caadar '((x (true y)))) true))
(assert (same? (caaddr '(x y (true))) true))
(assert (same? (cadaar '(((x true)))) true))
(assert (same? (cadadr '(x (y true))) true))
(assert (same? (caddar '((x y true))) true))
(assert (same? (cadddr '(x y z true)) true))
(assert (= (cdaaar '((((x true))))) '(true)))
(assert (= (cdaadr '(x ((y true)))) '(true)))
(assert (= (cdadar '((x (y true)))) '(true)))
(assert (= (cdaddr '(x y (z true))) '(true)))
(assert (= (cddaar '(((x y true)))) '(true)))
(assert (= (cddadr '(x (y z true))) '(true)))
(assert (= (cdddar '((x y z true))) '(true)))
(assert (= (cddddr '(w x y z true)) '(true)))

;;; nil?
(assert (nil? nil))
(assert !(nil? true))

;;; /=
(assert (/= 1 2))
(assert !(/= 1 1))

;;; type?
(assert (type? 1 :number))
(assert !(type? :keyword :number))

;;; list?
(assert !(list? 1))
(assert (list? (cons 1 nil)))
(assert (list? nil))

;;; all-satisfy?
(assert (all-satisfy? '(1 2 3 4 5) (lambda (x) (type? x :number))))
(assert !(all-satisfy? '(1 :a 3 :b 5) (lambda (x) (type? x :number))))

;;; any-satisfy?
(assert (any-satisfy? '(1 2 3 4 5) (lambda (x) (type? x :number))))
(assert (any-satisfy? '(1 :a 3 :b 5) (lambda (x) (type? x :number))))

;;; each-pair-satisfy?
(assert (each-pair-satisfy? '(1 2 3 4 5) <))
(assert !(each-pair-satisfy? '(1 2 3 3 5) <))

; }}}

(print :finish)
