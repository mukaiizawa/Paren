; paren core library.
1
(<- a 1)
(lambda (:opt a) x)

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
:xint_equal
(xint_equal 10 2x1010)
(xint_equal 1 0)
:xint_add
(xint_add 1 2)
(xint_add 1 (xint_add 3 0x30))
:xint_lt
(xint_lt 1 1)
(xint_lt 1 2)
(xint_lt 2 1)

;; cons
:cons
(cons :dot :list)
(cons 1 (cons 2 (cons 3 nil)))

;; car
:car
(car (cons :car :cdr))
(car (cons :car :cdr) :x)

;; cdr
:cdr
(cdr (cons :car :cdr))
(cdr (cons :car :cdr) :x)

; evalueate
((lambda (x) x) :identity)
((lambda (x y) (cons y x)) 1 2)

(<- a :outer)
((lambda (a) (<- a :inner) a) (quote a))
a

(lambda (x) a b c d)

:finish
