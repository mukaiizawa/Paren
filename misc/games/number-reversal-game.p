; number reversal game.

(import :console)
(import :rand)

(function input ()
  (catch
    (begin
      (print "1-9 >> ")
      (let (val (int (read-line)))
        (if (<= 1 val 9) val
            (raise ArgumentError))))
    (f (e)
      (if (! (is-a? e Error)) (throw e)
          (input)))))

(function! main (args)
  (let (n 0 numbers (rand.shuffle! (.. 1 10)))
    (while (! (apply <= numbers))
      (console.clear)
      (printf "times: %d\n" n)
      (println numbers)
      (let ((x y) (split-at numbers (input)))
        (<- numbers (concat (reverse! x) y)
            n (++ n)))
      (println "Congratulations!"))))
