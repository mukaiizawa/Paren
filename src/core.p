; paren core library.

;; list
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

(macro backquote (:rest body) 1)

(macro function (name args :rest body)
  (list <- name (cons lambda (cons args body))))

(macro begin-if (test :rest body)
  (list if test (cons begin body)))

(macro or (:rest expr)
  (begin-if expr
    (list if (car expr) true (cons or (cdr expr)))))

(macro and (:rest expr)
  (if (nil? (cdr expr))
    (car expr)
    (list if (car expr) (cons and (cdr expr)))))

; todo
; (function nth (lis n)
;   (if (<= n 0)
;     (car lis)
;     (nth (cdr lis) (-- n))))

(function nil? (x)
  (same? x nil))

(function not (x)
  (if x nil true))

(function map (lis f)
  (if lis (cons (f (car lis)) (map (cdr lis) f))))

(function cons? (x)
  (not (atom? x)))

(function list? (x)
  (or (nil? x) (cons? x)))

`(1 2 ,3)

; (let ((x y))
; (macro let (args :rest body)
;   (lambda (args

; (function reduce (lis f :key (identity nil identity?))
;   )
; (reduce '(1 2 3) (lambda (x y) (cons x y)))

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
