; if

(print (same? (if nil :x) nil))
(print (same? (if true :x) :x))
(print (same? (if nil :x
                  :y)
              :y))
(print (same? (if nil :x
                  nil :y)
              nil))
(print (same? (if nil :x
                  true :y
                  :z)
              :y))
(print (same? (if nil :x
                  nil :y
                  :z)
              :z ))
