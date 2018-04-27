; closure

(<- inc ((lambda (x) (lambda () (<- x (%xint_add x 1)))) 0))
(inc)
(inc)
(inc)
