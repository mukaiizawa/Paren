; tac.

(function! main (args)
  (foreach write-line
           (reverse! (collect read-line))))
