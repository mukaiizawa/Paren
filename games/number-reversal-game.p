; number reversal game.

(import :console)
(import :rand)

(function input ()
  (catch (Error (f (e) (input)))
    (write-bytes ">> ")
    (let (val (int (read-line)))
      (if (<= 1 val 9) val
          (raise Error)))))
 
(function! main (args)
  (let (n 0 numbers (rand.shuffle! (.. 1 10)))
    (while (! (apply <= numbers))
      (console.clear)
      (write-line (format "times: %d" n))
      (write numbers)
      (let (i (input))
        (<- numbers (concat (reverse! (slice numbers 0 i)) (slice numbers i))))
      (<- n (++ n)))
    (write-line "Congratulations!")))
