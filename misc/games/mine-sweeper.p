; mine sweeper.

(import :matrix)
(import :rand)

(class Cell ()
  state mine?)

(method Cell .init (:key mine?)
  (<- self->mine? mine?
      self->state :close)
  self)

(method Cell .mine? ()
  self->mine?)

(method Cell .open! ()
  (<- self->state :open))

(method Cell .close! ()
  (<- self->state :close))

(method Cell .mark! ()
  (<- self->state :mark))

(method Cell .open? ()
  (== self->state :open))

(method Cell .close? ()
  (== self->state :close))

(method Cell .mark? ()
  (== self->state :mark))

(class GameExit (SystemExit))

(class Game ()
  height
  width
  mine-count
  mark-count
  covered-count
  board)

(method Game .init (height width)
  (rand.seed (time))
  (<- self->height height
      self->width width
      self->covered-count (* height width)
      self->mark-count 0
      self->mine-count (// (* self->covered-count 0.2))
      self->board (matrix (list height width)))
  (let (n self->mine-count)
    (dolist (p (rand.shuffle! (product (.. 0 height) (.. 0 width))))
      (.put self->board p (.init (.new Cell) :mine? (>= (<- n (-- n)) 0)))))
  self)

(method Game .neighbors (p)
  (keep (f (q)
          (let (r (map + p q))
            (if (.inside? self->board r) r)))
        '((-1 -1) (-1 0) (-1 1) (0 -1)
                  (0 1) (1 -1) (1 0) (1 1))))

(method Game .count-neighbor-mines (p)
  (count .mine?
         (map (partial .at self->board)
              (.neighbors self p))))

(method Game .input ()
  (catch
    (begin
      (println "s(weep) x y")
      (println "m(ark) x y")
      (print ">> ")
      (let ((cmd x y) (reject empty? (split (read-line) " ")) p (map int (list x y)))
        (if (! (.inside? self->board p)) (raise ArgumentError "illegal coordinates")
            (prefix? "sweep" cmd) (list :sweep p)
            (prefix? "mark" cmd) (list :mark p)
            (raise ArgumentError "unkown command"))))
    (f (e)
      (if (! (is-a? e ArgumentError)) (throw e))
      (println (.to-s e) ", again")
      (.input self))))

(method Game .sweep (p)
  (let (*p (.at self->board p))
    (if (.mine? *p) (raise GameExit "game over")
        (.open? *p) (return nil)
        (.mark? *p) (<- self->mark-count (-- self->mark-count)))
    (.open! *p)
    (if (= (<- self->covered-count (-- self->covered-count)) self->mine-count) (raise GameExit "win")
        (= (.count-neighbor-mines self p) 0) (foreach (partial .sweep self) (.neighbors self p)))))

(method Game .mark (p)
  (let (*p (.at self->board p))
    (if (.close? *p) (begin (.mark! *p) (<- self->mark-count (++ self->mark-count)))
        (.mark? *p) (begin (.close! *p) (<- self->mark-count (-- self->mark-count))))))

(method Game .step ()
  (let ((cmd p) (.input self))
    (if (= cmd :sweep) (.sweep self p)
        (.mark self p))
    (.step (.show self))))

(method Game .show (:opt display-mine?)
  (printf "mines: %d, marked: %d, covered: %d\n" self->mine-count self->mark-count self->covered-count)
  (print "  ") (dotimes (y self->width) (printf "%2d" y))
  (println)
  (domatrix (p self->board)
            (let ((x y) p *p (.at self->board p))
              (if (= y 0) (printf "%2d" x))
              (printf "%2s" (if (.mine? *p) (if display-mine? "*" "#")
                                (.mark? *p) "?"
                                (.close? *p) "#"
                                (let (n (.count-neighbor-mines self p))
                                  (if (= n 0) "." (str n)))))
              (if (= y (-- self->width)) (println))))
  self)

(method Game .start ()
  (catch
    (.step (.show self))
    (f (e)
      (if (! (is-a? e GameExit)) (throw e))
      (.show self :display-mine)
      (println (.to-s e)))))

(function! main (args)
  (let ((:opt height width) (map int args))
    (.start (.init (.new Game) (|| height 6) (|| width 5)))))
