; sort.

(function! main (args)
  (foreach write-line (sort! (collect read-line))))
