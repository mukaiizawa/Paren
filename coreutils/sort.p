; sort.

(function! main (args)
  (foreach write-line
           (sort! (collect read-line) :sorter (f (x y) (< (memcmp x y) 0)))))
