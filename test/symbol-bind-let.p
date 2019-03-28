; symbol-bind/let

(symbol-bind a :v)

(print (same? a :v))

(let ()
  (print (same? a :v)))

(let (a :x b :y c :z)
  (print (same? a :x))
  (print (same? b :y))
  (print (same? c :z)))

(print (same? a :v))
