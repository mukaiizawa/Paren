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

; (macro begin0 (:rest body)
;   (print (car body))
;   (print (cdr body))
;   (let ((sym (gensym)))
;     (print 
;       (cons let (cons (list (list sym (car body)))
;                       (cons (cdr body)
;                             (list sym)))))))

(macro or (:rest expr)
  (if expr (list if (car expr) (car expr) (cons or (cdr expr)))))

(macro and (:rest expr)
  (if (nil? (cdr expr)) (car expr)
    (list if (car expr) (cons and (cdr expr)))))

(macro assert (test)
  (list begin-if (list not test)
        (list print (list list :AssertionFailed (list quote test)))
        '(quit)))

; fundamental function
(function error (:rest args)
  (print (cons :Error args))
  (quit))

(macro precondition (test)
  (list if (list not test) (list error :PreconditionError (list quote test))))

(macro postcondition (test)
  (list if (list not test) (list error  :PostconditionError (list quote test))))

(function identity (x) x)

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
(function nth (lis n)
  (precondition (and (list? lis) (< n (length lis))))
  (car (nthcdr lis n)))

(function nthcdr (lis n)
  (cond ((nil? lis) nil)
        ((<= n 0) lis)
        (:default (nthcdr (cdr lis) (-- n)))))

(function sublist (lis s :opt e)
  (let ((len (length lis))
        (e (or e len))
        (rec (lambda (lis n)
               (if (= n 0) nil
                 (cons (car lis) (rec (cdr lis) (-- n)))))))
    (precondition (and (>= s 0) (<= s e) (<= e len)))
    (rec (nthcdr lis s) (- e s))))

(function copy-list (lis)
  (sublist lis 0 (length lis)))

(function last-cons (lis)
  (precondition (list? lis))
  (if (nil? lis) nil
    (let ((rec (lambda (lis) (if (cdr lis) (rec (cdr lis)) lis))))
      (rec lis))))

(function last (lis)
  (precondition (list? lis))
  (car (last-cons lis)))

(function length (lis)
  (precondition (list? lis))
  (let ((rec (lambda (lis n)
               (if (nil? lis) n (rec (cdr lis) (++ n))))))
    (rec lis 0)))

(function append (lis :rest args)
  (precondition (and (list? lis) (all-satisfy? args list?)))
  (reduce args (lambda (x y)
                 (cdr (last-cons x)
                      (if (list? y) (copy-list y) (->list y)))
                 x)
          :identity (copy-list lis)))

(function add (lis x)
  (precondition (list? lis))
  (cdr (last-cons lis) (cons x nil))
  lis)

(macro push (lis x)
  (list begin
        (list precondition (list type? (list quote lis) :symbol))
        (list <- lis (list cons x lis))
        :SideEffects))

(macro pop (lis)
  (precondition (type? lis :symbol))
  (let ((g (gensym)))
    (begin (list precondition (list type? (list quote lis) :symbol))
           (list let (list (list g (list car lis)))
                 (list <- lis (list cdr lis))
                 g))))

(macro queue (lis x)
  (list push lis x))

(macro dequeue (lis)
  (list cdr (list last-cons lis) nil))

(function ->list (x)
  (if (list? x) x (list x)))

(function flatten (lis)
  (precondition (list? lis))
  (let ((acc nil)
        (rec (lambda (x)
               (cond ((nil? x) (reverse acc))
                     ((atom? x) (push acc x))
                     (true (if (nil? (car x)) (push acc nil)
                             (rec (car x)))
                           (rec (cdr x)))))))
    (rec lis)))

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
  (let ((rec (lambda (tree level)
               (if (atom? tree) (list quote tree)
                 (cons list (map tree (lambda (x)
                                        (list quote x))))))))
    (rec expr 1)))
; (<- a 3)
; (print ``(1 2 3))

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
(assert (same? (caar '((z))) 'z))
(assert (same? (cadr '(x z)) 'z))
(assert (= (cdar '((x z))) '(z)))
(assert (= (cddr '(x x z)) '(z)))
(assert (same? (caaar '(((z)))) 'z))
(assert (same? (caadr '(x (z))) 'z))
(assert (same? (cadar '((x z))) 'z))
(assert (same? (caddr '(x x z)) 'z))
(assert (= (cdaar '(((x z)))) '(z)))
(assert (= (cdadr '(x (x z))) '(z)))
(assert (= (cddar '((x x z))) '(z)))
(assert (= (cdddr '(x x x z)) '(z)))
(assert (same? (caaaar '((((z))))) 'z))
(assert (same? (caaadr '(x ((z)))) 'z))
(assert (same? (caadar '((x (z)))) 'z))
(assert (same? (caaddr '(x x (z))) 'z))
(assert (same? (cadaar '(((x z)))) 'z))
(assert (same? (cadadr '(x (x z))) 'z))
(assert (same? (caddar '((x x z))) 'z))
(assert (same? (cadddr '(x x x z)) 'z))
(assert (= (cdaaar '((((x z))))) '(z)))
(assert (= (cdaadr '(x ((x z)))) '(z)))
(assert (= (cdadar '((x (x z)))) '(z)))
(assert (= (cdaddr '(x x (x z))) '(z)))
(assert (= (cddaar '(((x x z)))) '(z)))
(assert (= (cddadr '(x (x x z))) '(z)))
(assert (= (cdddar '((x x x z))) '(z)))
(assert (= (cddddr '(x x x x z)) '(z)))

;;; identity
(assert (same? (identity :a) :a))

;;; not
(assert (same? !'x nil))
(assert (same? !nil true))

;;; /=
(assert (/= 1 2))
(assert !(/= 1 1))

;;; nil?
(assert (nil? nil))
(assert !(nil? true))

;;; type?
(assert (type? 1 :number))
(assert !(type? :keyword :number))

;;; cons?
(assert !(cons? 1))
(assert !(cons? nil))
(assert (cons? '(1)))

;;; list?
(assert !(list? 1))
(assert (list? nil))
(assert (list? '(1)))

;;; all-satisfy?
(assert (all-satisfy? '(1 2 3 4 5) (lambda (x) (type? x :number))))
(assert !(all-satisfy? '(1 :a 3 :b 5) (lambda (x) (type? x :number))))

;;; any-satisfy?
(assert (any-satisfy? '(1 2 3 4 5) (lambda (x) (type? x :number))))
(assert (any-satisfy? '(1 :a 3 :b 5) (lambda (x) (type? x :number))))

;;; each-pair-satisfy?
(assert (each-pair-satisfy? '(1 2 3 4 5) <))
(assert !(each-pair-satisfy? '(1 2 3 3 5) <))

;;; nth
(assert (= (nth '(1 2 3) 0) 1))

;;; nthcdr
(assert (= (nthcdr '(1 2 3) 1) '(2 3)))

;;; sublist
(assert (= (sublist '(1 2 3) 1) '(2 3)))
(assert (= (sublist '(1 2 3) 1 2) '(2)))

;;; copy-list
(assert (= (copy-list '(1 2 3)) '(1 2 3)))

;;; last-cons
(assert (= (last-cons '(1 2 3)) '(3)))

;;; last
(assert (= (last '(1 2 3)) 3))

;;; length
(assert (= (length '(1 2 3)) 3))

;;; append
(assert (= (append '(1) '(2) '(3)) '(1 2 3)))

;;; add
(assert (= (add (add '(1) 2) 3) '(1 2 3)))

;;; push/pop
(let ((lis '(1)))
  (push lis 2)
  (push lis 3)
  (assert (= lis '(3 2 1)))
  (assert (= (pop lis) 3)))

;;; flatten
(assert (= (flatten '(1 (2) (3 4))) '(1 2 3 4)))
(assert (= (flatten '(1 (nil) 2)) '(1 nil 2)))

; }}}

(print :finish)
