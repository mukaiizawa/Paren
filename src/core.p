; paren core library.

; primitive
;; xint
:%xint_equal
(%xint_equal 10 2x1010)
(%xint_equal 1 0)
:%xint_add
(%xint_add 1 2)
(%xint_add 1 0x30)

;; lambda
(%lambda (x) x)
(%lambda (x y z) x)
(%lambda (:opt a) x)
(%lambda (:opt a b) x)
(%lambda (:opt (a 0)) x)
(%lambda (:opt a (b 0)) x)
(%lambda (:opt (a 0) (b 0)) x)
(%lambda (:rest a) x)
(%lambda (:key a) x)
(%lambda (:key a b) x)
(%lambda (:key (a 0)) x)
(%lambda (:key a (b 0)) x)
(%lambda (:key (a 0) (b 0)) x)
(%lambda (x y :opt (a 0) (b 0)) x)
(%lambda (:rest a :key (a 0) (b 0) ) x)
(%lambda (x y :opt (a 0) (b 0) :rest a) x)
(%lambda (x y :opt (a 0) (b 0) :rest a :key a b (c 3)) x)

:finish
