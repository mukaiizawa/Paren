; return

(print (same? ((lambda () :x (return :y) :z)) :y))
