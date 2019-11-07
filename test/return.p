; return

(assert (same? ((lambda () :x (return :y) :z)) :y))
