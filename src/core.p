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

; association list
(function {} (alis key :opt (val nil val?))
  (find alis key))

(function in? (alis key)
  (and alis
       (or (= key (caarr alis))
           (in? (cdr alis) key))))

(function keys (alis)
  (map alis car))

(function vals (alis)
  (map alis cdr))

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

; should be implement
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
