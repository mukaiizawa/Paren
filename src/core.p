#|
paren core routine.

(def stream)
(<- stream (open "core.p"))
(eval (read stream))

|#

(print stdout "hello paren!!")

(def double)
(<- double (fn (:Number n)
             (* n n)))

(double 10)
