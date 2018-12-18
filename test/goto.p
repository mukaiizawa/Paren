; goto statement

(<- x 0 y 0)
(labels :x (<- x (+ x 1))
        :y (if (< x 3) (goto :x))
        (if (= y 0) (begin (<- y (+ y 1)) (goto :x))))

(print (= x 4))
