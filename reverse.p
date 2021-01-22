; output standard input in reverse order.

(function! main (args)
  (foreach write-line
           (reverse! (collect read-line))))
