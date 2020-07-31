; n queen problem

(import :point)
(import :matrix)

(<- $board-size 8)

(function show (board)
  (domatrix (p board)
    (if (= (.y p) 0) (write-line))
    (if (.at board p) (write-bytes "X")
        (write-bytes "-")))
  (write-line))

(function putable? (board p)
  (let (x (.x p) y (.y p))
    (dotimes (y $board-size)
      (if (.at board (Point.of x y)) (return nil)))
    (for (q p) (.inside? board q) (<- q (Point.of (-- (.x q)) (-- (.y q))))
      (if (.at board q) (return nil)))
    (for (q p) (.inside? board q) (<- q (Point.of (++ (.x q)) (-- (.y q))))
      (if (.at board q) (return nil)))
    true))

(function put-queen (board y)
  (if (= y $board-size) (show board)
      (dotimes (x $board-size)
        (let (p (Point.of x y))
          (when (putable? board p)
            (.put board p true)
            (put-queen board (++ y))
            (.put board p nil))))))

(function! main ()
  (let (board (.init (.new Matrix) :point (Point.of $board-size $board-size)))
    (put-queen board 0)))
