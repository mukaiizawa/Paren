; begin

(assert (same? (begin) nil))
(assert (same? (begin :x :y :z) :z))
