; symbol-bind/let

(<- a :v)

(assert (same? a :v))

(let ()
  (assert (same? a :v)))

(let (a :x b :y c :z)
  (assert (same? a :x))
  (assert (same? b :y))
  (assert (same? c :z)))

(assert (same? a :v))
