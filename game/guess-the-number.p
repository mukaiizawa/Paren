; Guess the number.

(<- $lower 0
    $upper 100)

(function input (n)
  (write-line (str "Is the number " n "? (l/h/y)"))
  (let (x (read))
    (if (in? x '(l h y)) x
        (input n))))

(function guess (lower upper)
  (let (middle (// (+ lower upper) 2) x (input middle))
    (if (= x 'h) (guess middle upper)
        (= x 'l) (guess lower middle))))

(function! main (args)
  (write-line (str "Choose a number between " $lower " and " $upper "."))
  (guess 0 100))
