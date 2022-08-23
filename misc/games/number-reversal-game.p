; number reversal game.

(import :console)
(import :rand)

(function input ()
  (catch
    (begin
      (write-bytes "1-9 >> ")
      (let (val (int (read-line)))
        (if (<= 1 val 9) val
            (raise ArgumentError))))
    (f (e) (input))))
 
(function! main (args)
  (let (n 0 numbers (rand.shuffle! (.. 1 10)))
    (while (! (apply <= numbers))
      (console.clear)
      (write-line (format "times: %d" n))
      (write numbers)
      (let ((x y) (split-at numbers (input)))
        (<- numbers (concat (reverse! x) y)
            n (++ n))))
    (write-line "Congratulations!")))
