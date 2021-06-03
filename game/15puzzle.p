; 15-puzzle game.

(import :matrix)
(import :optparse)
(import :point)
(import :rand)

(<- $vector
    ; symbol command unit-vector
    (list :right 'h (point  0 -1)
          :down  'j (point  1  0)
          :up    'k (point -1  0)
          :left  'l (point  0  1)))

(function show (board p)
  (if (= $hostname :windows) (system "cls"))
  (domatrix (q board)
    (write-bytes (if (.eq? p q) "[  ]" (format "[%-2d]" (.at board q))))
    (if (= (++ (.y q)) (.width board)) (write-line))))

(function move (board p dir)
  (let (q (.add p dir))
    (if (.inside? board q)
        (begin
          (.put board p (.at board q))
          (.put board q (exp (.width board) 2))
          (list board q))
        (list board p))))

(function input ()
  (write (except object? $vector))
  (let (selection (read))
    (|| (find (f (x) (if (= (cadr x) selection) (caddr x)))
              (group $vector 3))
        (input))))

(function in-game? (board)
  (let (i 0)
    (domatrix (q board)
      (if (!= (.at board q) (<- i (++ i))) (return true)))))

(function loop (board p)
  (show board p)
  (when (in-game? board)
    (apply loop (move board p (input)))))

(function init (n)
  (let (i 0 n (if (> n 0) n 4) board (.init (.new Matrix) (point n n)) p (point (-- n) (-- n)))
    (domatrix (q board)
      (.put board q (<- i (++ i))))
    (dotimes (i 1000)
      (<- (board p) (move board p (rand.choice (select object? $vector)))))
    (list board p)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args))
    (apply loop (init (int (.get op "n"))))))
