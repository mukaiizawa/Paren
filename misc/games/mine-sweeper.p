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
  state mine?)

(method Cell .init ()
  (.state! self :close))

(method Cell .state ()
  self->state)

(method Cell .state! (state)
  (assert (in? state '(:open :close :mark)))
  (<- self->state state)
  self)

(method Cell .set-mine! ()
  (<- self->mine? true))

(method Cell .mine? ()
  self->mine?)

(function cell (p)
  (if (.inside? $board p) (.at $board p)))

(function put-mine (p)
  (.set-mine! (cell p)))

(function mine? (p)
  (.mine? (cell p)))

(function open? (p)
  (== (.state (cell p)) :open))

(function close? (p)
  (== (.state (cell p)) :close))

(function mark? (p)
  (== (.state (cell p)) :mark))

(function open! (p)
  (assert (! (open? p)))
  (if (mark? p) (<- $mark-count (-- $mark-count)))
  (<- $covered-count (-- $covered-count))
  (.state! (cell p) :open))

(function close! (p)
  (assert (! (open? p)))
  (if (mark? p) (<- $mark-count (-- $mark-count)))
  (.state! (cell p) :close))

(function mark! (p)
  (assert (! (open? p)))
  (if (! (mark? p)) (<- $mark-count (++ $mark-count)))
  (.state! (cell p) :mark))

; game

(function neighbor-coods (p)
  (select cell
          (map (f (q) (map + p q))
               '((-1 -1) (-1 0) (-1 1) (0 -1)
                         (0 1) (1 -1) (1 0) (1 1)))))

(function count-neighbor-mine (p)
  (count mine? (neighbor-coods p)))

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
        (if (zero? (count-neighbor-mine p)) (foreach sweep (neighbor-coods p))))))

(function show ()
  (console.clear)
  (write (list :mines $mine-count :marked $mark-count :covered $covered-count))
  (write-bytes "  ") (dotimes (y $width) (write-bytes (format "%-2d" y)))
  (write-line)
  (dotimes (x $height)
    (dotimes (y $width)
      (if (= y 0) (write-bytes (format "%-2d" x)))
      (let (p (list x y))
        (write-bytes (format "%-2s"
                             (if (close? p) "#"
                                 (mark? p) "?"
                                 (let (n (count-neighbor-mine p))
                                   (if (= n 0) "."
                                       (str n))))))))
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

(function step ()
  (show)
  (if (solved?) (write-line "win")
      (let ((cmd p) (input))
        (if (= cmd :sweep) (sweep p)
            (= cmd :mark) (mark p)
            (assert nil))
        (step))))

(function init ()
  (let (fill-mine (f (n)
                    (if (> n 0)
                        (let (p (list (rand.int $height) (rand.int $width)))
                          (if (mine? p) (fill-mine n)    ; collision
                              (begin
                                (put-mine p)
                                (fill-mine (-- n))))))))
  (rand.seed (time))
  (domatrix (p $board) (.put $board p (.new Cell)))
  (fill-mine $mine-count)))

(function! main (args)
  (init)
  (step))
