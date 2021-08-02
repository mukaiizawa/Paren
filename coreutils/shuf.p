; shuf.

(import :rand)

(function! main (args)
  (foreach write-line
           (rand.shuffle! (collect read-line))))
