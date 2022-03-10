; N-Queens problem.

(import :matrix)

(<- $size 8
    $board (matrix (list 8 8)))

(function show ()
  (domatrix (p $board)
    (if (= (cadr p) 0) (write-line))
    (write-bytes (if (.at $board p) "Q" "*")))
  (write-line))

(function putable? (p)
  (let ((x y) p)
    (dotimes (y $size)
      (if (.at $board (list x y)) (return nil)))
    (for (q p) (.inside? $board q) (q (map + q '(-1 -1)))
      (if (.at $board q) (return nil)))
    (for (q p) (.inside? $board q) (q (map + '(1 -1)))
      (if (.at $board q) (return nil)))
    true))

(function put-queen (y)
  (if (= y $size) (show)
      (dotimes (x $size)
        (let (p (list x y))
          (when (putable? p)
            (.put $board p true)
            (put-queen (++ y))
            (.put $board p nil))))))

(function! main (args)
  (put-queen 0))
