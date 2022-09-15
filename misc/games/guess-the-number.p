; Guess the number.

(<- $min 0
    $max 100
    $selection '(h l e))

(function input (n)
  (println "Is the number " n "? " $selection)
  (let (x (read))
    (if (in? x $selection) x
        (input n))))

(function guess (l h)
  (let (m (// (+ l h) 2) x (input m))
    (if (= x 'h) (guess (++ m) h)
        (= x 'l) (guess l (-- m)))))

(function! main (args)
  (println "Choose a number between " $min " and " $max ".")
  (guess 0 100))
