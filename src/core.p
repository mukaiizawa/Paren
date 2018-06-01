; paren core library.

;; list
(<- caar (lambda (x) (car (car x)))
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

(<- nil? (lambda (x) (same? x nil))
    not (lambda (x) (if x false true))
    list (lambda (:rest args) args))

(<- reduce (lambda (lis f :key (identity nil identity?))
             identity?
             ))
(reduce '(1 2 3) (lambda (x y) (cons x y)))

; (defun reduce (function sequence &key from-end (start 0)
;                         end (initial-value nil ivp) key)
;   "The specified Sequence is ``reduced'' using the given Function.
;   See manual for details."
;   (unless end (setq end (length sequence)))
;   (if (= end start)
;     (if ivp initial-value (funcall function))
;     (seq-dispatch
;      sequence
;      (if from-end
;        (list-reduce-from-end  function sequence start end initial-value ivp key)
;        (list-reduce function sequence start end initial-value ivp key))
;      (let* ((disp (if from-end -1 1))
;             (index (if from-end (1- end) start))
;             (terminus (if from-end (1- start) end))
;             (value (if ivp initial-value
;                        (let ((elt (aref sequence index)))
;                          (setq index (+ index disp))
;                          (if key (funcall key elt) elt))))
;             (element nil))
;        (do* ()
;             ((= index terminus) value)
;          (setq element (aref sequence index)
;                index (+ index disp)
;                element (if key (funcall key element) element)
;                value (funcall function (if from-end element value) (if from-end value element))))))))

(<- append (lambda (lis :rest args)))

; (<- function (macro (name args :rest body)
;                (list <- name (append (list lambda args) body))))
; (function ++ (x) (<- x (xint_add x 1)))

; primitive
;; <-
(<- a 1 b 2 c 3)
a
b
c

;; if
(if true true false)
(if false true false)
(if false true)

;; quote
(quote hello_paren)

;; lambda
(lambda (x) x)
(lambda (x y z) x)
(lambda (:opt a) x)
(lambda (:opt a b) x)
(lambda (:opt (a 0)) x)
(lambda (:opt a (b 0)) x)
(lambda (:opt (a 0) (b 0)) x)
(lambda (:rest a) x)
(lambda (:key a) x)
(lambda (:key a b) x)
(lambda (:key (a 0)) x)
(lambda (:key a (b 0)) x)
(lambda (:key (a 0) (b 0)) x)
(lambda (x y :opt (a 0) (b 0)) x)
(lambda (:rest a :key (a 0) (b 0)) x)
(lambda (x y :opt (a 0) (b 0) :rest a) x)
(lambda (x y :opt (a 0) (b 0) :rest a :key a b (c 3)) x)

;; xint
:xint_eq
(xint_eq 10 2x1010)
(xint_eq 1 0)

:xint_add
(xint_add 1 2)
(xint_add 1 (xint_add 3 0x30))

:xint_lt
(xint_lt 1 1)
(xint_lt 1 2)
(xint_lt 2 1)

;; list
:cons
(cons :dot :list)
(cons 1 (cons 2 (cons 3 nil)))

:car
(car (cons :car :cdr))
(car (cons :car :cdr) :x)

:cdr
(cdr (cons :car :cdr))
(cdr (cons :car :cdr) :x)

:same?
(same? nil nil)
(same? nil false)

; evalueate
((lambda (x) x) :identity)
((lambda (x y) (cons y x)) 1 2)

(<- a :outer)
((lambda (a) (<- a :inner) a) (quote a))
a

(lambda (x) a b c d)

:finish
