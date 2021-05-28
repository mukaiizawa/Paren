; Conway's Game of Life.

(import :matrix)
(import :point)
(import :rand)

(<- $size 10)

(function show (board)
  (sleep 1)
  (if (= $hostname :windows) (system "cls"))
  (domatrix (p board)
    (if (= (.y p) 0) (write-line))
    (write-bytes (if (.at board p) "#" " "))))

(function neighbor-count (board p)
  (len (except nil? (map (f (xy)
                           (let (r (.add p (apply point xy)))
                             (&& (.inside? board r) (.at board r))))
                         '((-1 -1) (-1  0) (-1  1)
                           ( 0 -1)         ( 0  1)
                           ( 1 -1) ( 1  0) ( 1  1))))))

(function first-generation (board)
  (rand.seed (time))
  (domatrix (p board) (.put board p (rand.bool)))
  board)

(function next-generation (curr next)
  (show curr)
  (domatrix (p curr)
    (let (n (neighbor-count curr p))
      (.put next p (|| (&& (.at curr p) (= n 2)) (= n 3)))))
  (next-generation next curr))

(function make-board (size)
  (.init (.new Matrix) (point $size $size)))

(function! main (args)
  (next-generation (first-generation (make-board $size)) (make-board $size)))
