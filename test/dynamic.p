; dynamic

(symbol-bind $x 0)

(symbol-bind xstatic
             (lambda ()
               (let (x $x)
                 x)))

(symbol-bind xdynamic
             (lambda ()
               (let (x (dynamic $x))
                 x)))

(print (number= (let ($x 1) (xstatic)) 0))
(print (number= (let ($x 1) (xdynamic)) 1))
