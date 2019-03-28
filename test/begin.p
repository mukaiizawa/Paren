; begin

(print (same? (begin) nil))

(print (same? (begin :x
                     :y
                     :z)
              :z))
