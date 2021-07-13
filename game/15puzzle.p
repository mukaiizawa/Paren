; 15-puzzle game.

(import :console)
(import :matrix)
(import :optparse)
(import :rand)

(<- $vector
    ; symbol command unit-vector
    (list :right 'h '( 0 -1)
          :down  'j '( 1  0)
          :up    'k '(-1  0)
          :left  'l '( 0  1)))

(function show (board p)
  (console.clear)
  (let ((row-counts col-counts) (.shape board))
    (dotimes (x row-counts)
      (dotimes (y col-counts)
        (write-bytes (if (= p (list x y)) "[  ]" (format "[%-2d]" (.at board (list x y)))))
        (if (= (++ y) row-counts) (write-line))))))

(function move (board p direction)
  (let (q (map + p direction))
    (if (.inside? board q)
        (begin
          (.put board p (.at board q))
          (.put board q (int (pow (car (.shape board)) 2)))
          (list board q))
        (list board p))))

(function input ()
  (write (except list? $vector))
  (let (selection (read))
    (|| (find (f (x) (if (= (cadr x) selection) (caddr x)))
              (group $vector 3))
        (input))))

(function in-game? (board)
  (let (i 0)
    (domatrix (q board)
      (if (!= (.at board q) (<- i (++ i))) (return true)))))

(function step (board p)
  (show board p)
  (when (in-game? board)
    (apply step (move board p (input)))))

(function init (n)
  (let (i 0 n (if (> n 0) n 4) board (matrix (list n n)) p (list (-- n) (-- n)))
    (domatrix (q board)
      (.put board q (<- i (++ i))))
    (dotimes (i 1000)
      (<- (board p) (move board p (rand.choice (select list? $vector)))))
    (list board p)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args))
    (apply step (init (int (.get op "n"))))))
