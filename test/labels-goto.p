; labels/goto

(symbol-bind x 0 y 0)

(labels :x (symbol-bind x (number+ x 1))
        :y (if (< x 3) (goto :x))
        (if (number= y 0)
            (begin (symbol-bind y (number+ y 1))
                   (goto :x))))

(print (number= x 4))
