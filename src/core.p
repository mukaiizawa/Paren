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
    cddddr (lambda (x) (cdr (cdddr x)))
    <-caar (lambda (x y) (<-car (car x) y))
    <-cadr (lambda (x y) (<-car (cdr x) y))
    <-cdar (lambda (x y) (<-cdr (car x) y))
    <-cddr (lambda (x y) (<-cdr (cdr x) y))
    <-caaar (lambda (x y) (<-car (caar x) y))
    <-caadr (lambda (x y) (<-car (cadr x) y))
    <-cadar (lambda (x y) (<-car (cdar x) y))
    <-caddr (lambda (x y) (<-car (cddr x) y))
    <-cdaar (lambda (x y) (<-cdr (caar x) y))
    <-cdadr (lambda (x y) (<-cdr (cadr x) y))
    <-cddar (lambda (x y) (<-cdr (cdar x) y))
    <-cdddr (lambda (x y) (<-cdr (cddr x) y))
    <-caaaar (lambda (x y) (<-car (caaar x) y))
    <-caaadr (lambda (x y) (<-car (caadr x) y))
    <-caadar (lambda (x y) (<-car (cadar x) y))
    <-caaddr (lambda (x y) (<-car (caddr x) y))
    <-cadaar (lambda (x y) (<-car (cdaar x) y))
    <-cadadr (lambda (x y) (<-car (cdadr x) y))
    <-caddar (lambda (x y) (<-car (cddar x) y))
    <-cadddr (lambda (x y) (<-car (cdddr x) y))
    <-cdaaar (lambda (x y) (<-cdr (caaar x) y))
    <-cdaadr (lambda (x y) (<-cdr (caadr x) y))
    <-cdadar (lambda (x y) (<-cdr (cadar x) y))
    <-cdaddr (lambda (x y) (<-cdr (caddr x) y))
    <-cddaar (lambda (x y) (<-cdr (cdaar x) y))
    <-cddadr (lambda (x y) (<-cdr (cdadr x) y))
    <-cdddar (lambda (x y) (<-cdr (cddar x) y))
    <-cddddr (lambda (x y) (<-cdr (cdddr x) y)))

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
  (not (atom? x)))

(function list? (x)
  (or (nil? x) (cons? x)))

; list processor
(function identity (x) x)

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

(function find (lis e :key (test same?) (key identity))
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
(function {} (alis key :key (test same?))
  (if (nil? alis)
    nil
    (let ((first (car alis))
          (rest (cdr alis)))
      (if (test key (car first))
        (cdr first)
        ({} (cdr alis) key :test test)))))

(function <-{} (alis key val :key (test same?))
  (if (nil? alis)
    nil
    (let ((first (car alis))
          (rest (cdr alis)))
      (if (test key (car first))
        (<-cdr first val)
        (<-{} (cdr alis) key val :test test)))))

(function has-key? (alis key :key (test same?))
  (and alis
       (or (test key (car (car alis)))
           (has-key? (cdr alis) key :test test))))

; numeric
(macro <-+ (x y)
  (list <- x (list + x y)))

(macro <-- (x y)
  (list <- x (list - x y)))

(function + (:rest args)
  (reduce args number_add :identity 0))

(function ++ (x)
  (number_add x 1))

(function - (:rest args)
  (reduce (map (cdr args) negated) number_add :identity (car args)))

(function -- (x)
  (number_add x -1))

(function * (:rest args) (reduce args number_multiply :identity 1))

(function negated (x)
  (* x -1))

(function = (:rest args)
  (if (nil? (cdr args))
    nil
    (let ((rec (lambda (args)
                 (cond ((nil? (cdr args)) (car args))
                       ((number_eq (car args) (cadr args)) (rec (cdr args)))
                       (:default nil)))))
      (rec args))))

(function < (x y)
  (number_lt x y))

(function > (x y)
  (< y x))

(function <= (x y)
  (not (< y x)))

(function >= (x y)
  (not (< x y)))

; test
(function hanoi (n)
  (let ((move (lambda (n a b)
                (if (> n 1) (move (-- n) a (- 6 a b)))
                (print (list 'move n 'from a 'to b))
                (if (> n 1) (move (-- n) (- 6 a b) b)))))
    (move n 1 2)))

(hanoi 3)

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
