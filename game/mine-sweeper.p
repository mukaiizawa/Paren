; mine sweeper.

(import :console)
(import :matrix)
(import :rand)

(<- $height 6 $width 5
    $cell-count (* $height $width)
    $mine-count (// (* $cell-count 0.2))
    $mark-count 0
    $covered-count $cell-count
    $board (matrix (list $height $width)))

; cell

(class Cell ()
  state mine? mine-counts)

(method Cell .init ()
  (&state! self :close)) ; (:open :close :mark)

(method Cell .to-s ()
  (format "%-2s"
          (if (= (&state self) :close) "#"
              (= (&state self) :mark) "?"
              (let (mine-counts (&mine-counts self))
                (if (= mine-counts 0) "."
                    (str mine-counts))))))

(function init-cell (p)
  (.put $board p (.new Cell)))

(function cell (p)
  (if (.inside? $board p) (.at $board p)))

(function neighbor-cells (p)
  (select (f (p) (cell p))
          (map (f (q) (map + p q))
               '((-1 -1) (-1 0) (-1 1) (0 -1)
                         (0 1) (1 -1) (1 0) (1 1)))))

(function set-mine-count (p)
  (&mine-counts! (cell p) (len (select mine? (neighbor-cells p)))))

(function open? (p)
  (= (&state (cell p)) :open))

(function close? (p)
  (= (&state (cell p)) :close))

(function mark? (p)
  (= (&state (cell p)) :mark))

(function mine? (p)
  (&mine? (cell p)))

(function open! (p)
  (assert (! (open? p)))
  (if (mark? p) (<- $mark-count (-- $mark-count)))
  (<- $covered-count (-- $covered-count))
  (&state! (cell p) :open))

(function close! (p)
  (assert (! (open? p)))
  (if (mark? p) (<- $mark-count (-- $mark-count)))
  (&state! (cell p) :close))

(function mark! (p)
  (assert (! (open? p)))
  (if (! (mark? p)) (<- $mark-count (++ $mark-count)))
  (&state! (cell p) :mark))

(function put-mine (p)
  (&mine?! (cell p) true))

(function no-mines-around? (p)
  (= (&mine-counts (cell p)) 0))

; game

(function mark (p)
  (if (mark? p) (close! p)
      (close? p) (mark! p)))

(function sweep (p)
  (if (mine? p)
      (begin
        (write-line "game over")
        (quit))
      (! (open? p))
      (begin
        (open! p)
        (if (no-mines-around? p)
            (foreach sweep (neighbor-cells p))))))

(function show ()
  (console.clear)
  (write (list :mines $mine-count :marked $mark-count :covered $covered-count))
  (write-bytes "  ") (dotimes (y $width) (write-bytes (format "%-2d" y)))
  (write-line)
  (dotimes (x $height)
    (dotimes (y $width)
      (if (= y 0) (write-bytes (format "%-2d" x)))
      (write-bytes (.to-s (cell (list x y)))))
    (write-line)))

(function input ()
  (write-line "command")
  (write-line "  s(weep) x y")
  (write-line "  m(ark) x y")
  (write-bytes ">> ")
  (let (cmd (read) x (read) y (read) p (list x y))
    (if (|| (! (int? x)) (! (int? y)) (nil? (cell p)))
        (begin
          (write-line "illegal coordinate")
          (input))
        (= cmd 's) (list :sweep p)
        (= cmd 'm) (list :mark p)
        (begin
          (write-line "illegal command")
          (input)))))

(function solved? ()
  (= $covered-count $mine-count))

(function loop ()
  (show)
  (if (solved?) (write-line "win")
      (let ((cmd p) (input))
        (if (= cmd :sweep) (sweep p)
            (= cmd :mark) (mark p)
            (assert nil))
        (loop))))

(function init ()
  (let (fill-mine (f (n)
                    (if (> n 0)
                        (let (p (list (rand.int $height) (rand.int $width)))
                          (if (mine? p) (fill-mine n)    ; collision
                              (begin
                                (put-mine p)
                                (fill-mine (-- n))))))))
  (rand.seed (time))
  (domatrix (p $board) (init-cell p))
  (fill-mine $mine-count)
  (domatrix (p $board) (set-mine-count p))))

(function! main (args)
  (init)
  (loop))
