; closure

:inc
(<- inc ((lambda (x) (lambda () (<- x (+ x 1)))) 0))
(inc)
(inc)
(inc)

:make-adder
(<- mkadder (lambda (x) (lambda (y) (+ x y)))
    adder3 (mkadder 3)
    adder5 (mkadder 5))
(adder3 9)
(adder5 9)
