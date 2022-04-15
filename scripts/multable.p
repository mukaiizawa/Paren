; multiplication table.

(function! main (args)
  (dotimes (i 20)
    (dotimes (j 20)
      (write-bytes (format "%4d" (apply * (map ++ (list i j))))))
    (write-line)))
