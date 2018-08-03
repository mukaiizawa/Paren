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
  (list if (list not test)
        (list print (list list :AssertionFailed (list quote test)))))

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

; pos

(<- Object '((:super nil)
             (:type :Object)))

(function . (object prop :opt (val nil val?))
  (let ((pair (find object prop :key car)))
    (if (nil? pair) (assert "can not find prop"))
    (if val? (cdr pair val) (cdr pair))))

(print (. Object :super)) 

; TODO
; (function even? (x))
; (function odd? (x))

; test
; {{{

;;; cxr
(assert (= (list 1 2 3) '(1 2 3)))
(assert (same? (caar '((caar) x)) 'caar))
(assert (same? (cadr '(x cadr)) 'cadr))
(assert (same? (cdar '((x . cdar) y)) 'cdar))
(assert (same? (cddr '(x y . cddr)) 'cddr))
(assert (same? (caaar '(((caaar)))) 'caaar))
(assert (same? (caadr '(x (caadr))) 'caadr))
(assert (same? (cadar '((x cadar) y)) 'cadar))
(assert (same? (caddr '(x y caddr)) 'caddr))
(assert (same? (cdaar '(((x . cdaar) . y) . cdr)) 'cdaar))
(assert (same? (cdadr '(x (y . cdadr))) 'cdadr))
(assert (same? (cddar '((x y . cddar))) 'cddar))
(assert (same? (cdddr '(x y z . cdddr)) 'cdddr))
(assert (same? (caaaar '((((caaaar))))) 'caaaar))
(assert (same? (caaadr '(x ((caaadr)))) 'caaadr))
(assert (same? (caadar '((x (caadar)))) 'caadar))
(assert (same? (caaddr '(x y (caaddr))) 'caaddr))
(assert (same? (cadaar '(((x cadaar)))) 'cadaar))
(assert (same? (cadadr '(x (y cadadr))) 'cadadr))
(assert (same? (caddar '((x y caddar))) 'caddar))
(assert (same? (cadddr '(x y z cadddr)) 'cadddr))
(assert (same? (cdaaar '((((caaar . cdaaar))))) 'cdaaar))
(assert (same? (cdaadr '(x ((y . cdaadr)))) 'cdaadr))
(assert (same? (cdadar '((x (y . cdadar)))) 'cdadar))
(assert (same? (cdaddr '(x y (z . cdaddr))) 'cdaddr))
(assert (same? (cddaar '(((x y . cddaar)))) 'cddaar))
(assert (same? (cddadr '(x (y z . cddadr))) 'cddadr))
(assert (same? (cdddar '((x y z . cdddar))) 'cdddar))
(assert (same? (cddddr '(w x y z . cddddr)) 'cddddr))

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

; should be implement
; {{{
; (<- $$backquote-depth 0)
; (<- q '(r s))
; ``(q ,q ,,q ,@q ,,'q ,',q ',,q ,@,q ,,@q ,@,@q)
;
; (CONS 'Q
;       (CONS Q
;             (CONS (R S)
;                   (APPEND Q
;                           (CONS Q
;                                 (CONS '(R S) (CONS (LIST 'QUOTE (R S)) (APPEND (R S) (LIST* R S (APPEND R S))))))))))
;                                 }}}
