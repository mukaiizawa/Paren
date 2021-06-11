; Knight tour problem.

(import :matrix)

(<- $size 5
    $size^2 (* $size $size))

(function show (board)
  (domatrix (p board)
    (if (= (cadr p) 0) (write-line))
    (write-bytes (format "%-3d" (.at board p))))
  (quit))    ; only the first one solution.

(function neighbors (p)
  (map (f (q) (map + p q))
       '((-2 1) (-2 -1) (2 1) (2 -1)
                (-1 2) (-1 -2) (1 2) (1 -2))))

(function move (board p n)
  (.put board p n)
  (if (= n $size^2) (show board)
      (dolist (q (neighbors p))
        (if (&& (.inside? board q) (nil? (.at board q))) (move board q (++ n)))))
  (.put board p nil))

(function! main (args)
  (let (board (matrix (list $size $size)))
    (domatrix (p board)
      (move board p 1))))
