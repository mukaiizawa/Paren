; n queen problem

(import :point)
(import :matrix)

(global-symbol $board-size 8)

(function show (board)
  (domatrix (p board)
    (if (= (.y p) 0) (write-line))
    (if (.at board p) (write-string "X")
        (write-string "-")))
  (write-line))

(function putable? (board p)
  (let (x (.x p) y (.y p))
    (dotimes (y $board-size)
      (if (.at board (point x y)) (return nil)))
    (for (q p) (.inside? board q) (<- q (point (-- (.x q)) (-- (.y q))))
      (if (.at board q) (return nil)))
    (for (q p) (.inside? board q) (<- q (point (++ (.x q)) (-- (.y q))))
      (if (.at board q) (return nil)))
    true))

(function put-queen (board y)
  (if (= y $board-size) (show board)
      (dotimes (x $board-size)
        (let (p (point x y))
          (when (putable? board p)
            (.at! board p true)
            (put-queen board (++ y))
            (.at! board p nil))))))

(function! main ()
  (let (board (.init (.new Matrix) :point (point $board-size $board-size)))
    (put-queen board 0)))
