; Knight tour problem.

(import :point)
(import :matrix)

(<- $size 5
    $size^2 (* $size $size))

(function show (board)
  (domatrix (p board)
    (if (= (.y p) 0) (write-line))
    (write-bytes (format "%-3d" (.at board p))))
  (quit))    ; only the first one solution.

(function next-points (p)
  (map (f (xy) (.add p (apply point xy)))
       '((-2 1) (-2 -1) (2 1) (2 -1)
                (-1 2) (-1 -2) (1 2) (1 -2))))

(function move (board p n)
  (.put board p n)
  (if (= n $size^2) (show board)
      (dolist (q (next-points p))
        (if (&& (.inside? board q) (nil? (.at board q))) (move board q (++ n)))))
  (.put board p nil))

(function! main (args)
  (let (board (.init (.new Matrix) (point $size $size)))
    (domatrix (p board)
      (move board p 1))))
