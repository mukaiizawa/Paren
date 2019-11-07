; if

(assert (same? (if nil :x) nil))
(assert (same? (if true :x) :x))
(assert (same? (if nil :x
                   :y)
               :y))
(assert (same? (if nil :x
                   nil :y)
               nil))
(assert (same? (if nil :x
                   true :y
                   :z)
               :y))
(assert (same? (if nil :x
                   nil :y
                   :z)
               :z ))
