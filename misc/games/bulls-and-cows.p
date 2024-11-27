; Bulls and cows.

(import :rand)
(import :console)

(<- $digit-count 4
    $guess-count 8)

(function input ()
  (console.write ">> ")
  (let (val (read-line))
    (if (! (digit? val)) (console.write "should be a number.\n")
        (!= (len val) $digit-count) (console.write "invalid length.\n")
        (return (map int (split val))))
    (input)))

(function count-cows (answer guess)
  (count (f (x) (in? x guess)) answer))

(function count-bulls (answer guess)
  (count (f (x) (apply = x)) (zip answer guess)))

(function show (answer guesses)
  (console.clear)
  (console.write "N Guess Bulls Cows\n")
  (console.write "--\n")
  (dotimes (i (len guesses))
    (let (guess ([] guesses i))
      (console.write (format "%d %s  %d     %d\n"
                             (++ i)
                             (apply str guess)
                             (count-bulls answer guess)
                             (count-cows answer guess)))))
  (console.write "--\n"))

(function! main (args)
  (let (guesses nil answer (slice (rand.shuffle! (.. 10)) 0 $digit-count))
    (dotimes (i $guess-count)
      (show answer (reverse guesses))
      (console.write "guess the numbers.\n")
      (let (guess (input))
        (push! guess guesses)
        (if (= guess answer) (break))))
    (show answer (reverse guesses))
    (if (in? answer guesses) (console.write "win")
        (begin
          (console.write "lose\n")
          (console.write (apply str answer))))))
