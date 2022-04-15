; evaluate code and output.

(function xeval ()
  (foreach write (map eval (collect read))))

(function! main (args)
  (if (nil? args) (xeval)
      (with-memory-stream ($in (join args " ")) (xeval))))
