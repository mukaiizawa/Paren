; dynamic

(<- $x 0)

(<-  xstatic
     (lambda ()
       (let (x $x)
         x)))

(<-  xdynamic
     (lambda ()
       (let (x (dynamic $x))
         x)))

(print (= (let ($x 1) (xstatic)) 0))
(print (= (let ($x 1) (xdynamic)) 1))
