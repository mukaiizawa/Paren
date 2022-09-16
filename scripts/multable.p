; multiplication table.

(function! main (args)
  (dotimes (i 20)
    (dotimes (j 20)
      (printf "%4d" (apply * (map ++ (list i j)))))
    (println)))
