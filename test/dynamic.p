; dynamic

(<- $x 0)

(<- xstatic
    (lambda ()
      (let (x $x)
        x)))

(<- xdynamic
    (lambda ()
      (let (x (dynamic $x))
        x)))

(assert (= (let ($x 1) (xstatic)) 0))
(assert (= (let ($x 1) (xdynamic)) 1))
