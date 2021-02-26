; output standard input in reverse order.

(function tac ()
  ; tac
  ; Output standard input in reverse order.
  (foreach write-line
           (reverse! (collect read-line))))

(function! main (args)
  (tac))
