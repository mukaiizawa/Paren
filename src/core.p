#|
paren core routine.

(def stream)
(<- stream (open "core.p"))
(eval (read stream))

|#

(defMacro twice (x)
  (list `progn x x))

(twice (print 1))

#|
(print stdout "hello paren!!")

(def double)
(<- double (fn (:Number n)
             (* n n)))
(double 10)

(def length)
(<- length (fn (lis)
             (ifElse lis (+ 1 (length (cdr lis))) 0)))
(length `(1 2 3 4))
|#
