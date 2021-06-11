; Conway's Game of Life.

(import :console)
(import :math)
(import :matrix)
(import :optparse)
(import :rand)

(<- $seeds #{
    ;; oscillator.
    blinker '(
              0 1 0
              0 1 0
              0 1 0
              )
    beacon '(
             1 1 0 0
             1 1 0 0
             0 0 1 1
             0 0 1 1
             )
    toad '(
           0 1 0 0
           0 1 1 0
           0 1 1 0
           0 0 1 0
           )
    pinwheel '(
             0 0 0 0 0 0 1 1 0 0 0 0
             0 0 0 0 0 0 1 1 0 0 0 0
             0 0 0 0 0 0 0 0 0 0 0 0
             0 0 0 0 1 1 1 1 0 0 0 0
             1 1 0 1 0 0 1 0 1 0 0 0
             1 1 0 1 0 1 0 0 1 0 0 0
             0 0 0 1 0 0 0 1 1 0 1 1
             0 0 0 1 0 0 0 0 1 0 1 1
             0 0 0 0 1 1 1 1 0 0 0 0
             0 0 0 0 0 0 0 0 0 0 0 0
             0 0 0 0 1 1 0 0 0 0 0 0
             0 0 0 0 1 1 0 0 0 0 0 0
             )
    galaxy '(
             0 0 0 1 0 0 1 0 0 0 0 0 0
             0 0 0 0 0 0 0 1 0 0 0 0 0
             0 0 0 0 0 0 0 0 1 1 0 0 0
             0 0 1 0 1 1 0 0 0 0 0 0 1
             0 0 1 0 0 1 0 0 0 1 0 0 0
             0 1 0 0 0 1 0 1 1 1 0 0 0
             1 0 0 0 0 0 0 0 0 0 0 0 1
             0 0 0 1 1 1 0 1 0 0 0 1 0
             0 0 0 1 0 0 0 1 0 0 1 0 0
             1 0 0 0 0 0 0 1 1 0 1 0 0
             0 0 0 1 1 0 0 0 0 0 0 0 0
             0 0 0 0 0 1 0 0 0 0 0 0 0
             0 0 0 0 0 0 1 0 0 1 0 0 0
             )
    ;; spaceship
    glider '(
             0 1 0 0 0 0 0 0 0 0
             0 0 1 0 0 0 0 0 0 0
             1 1 1 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0
             )
    })

(function show (universe)
  (sleep 1)
  (console.clear)
  (domatrix (p universe)
    (if (= (cadr p) 0) (write-line))
    (write-bytes (if (.at universe p) "#" " "))))

(function neighbor-count (universe p)
  (len (except nil? (map (f (q)
                           (let (r (map + p q))
                             (&& (.inside? universe r) (.at universe r))))
                         '((-1 -1) (-1  0) (-1  1)
                           ( 0 -1)         ( 0  1)
                           ( 1 -1) ( 1  0) ( 1  1))))))

(function first-generation (universe :opt args)
  (if (nil? args)
      (begin
        (rand.seed (time))
        (domatrix (p universe) (.put universe p (rand.bool))))
      (let ((rows cols) (.shape universe))
        (dotimes (x rows)
          (dotimes (y cols)
            (.put universe (list x y) (= (car args) 1))
            (<- args (cdr args))))))
  universe)

(function next-generation (curr next)
  (show curr)
  (domatrix (p curr)
    (let (n (neighbor-count curr p))
      (.put next p (|| (&& (.at curr p) (= n 2)) (= n 3)))))
  (next-generation next curr))

(function! main (args)
  ; game-of-life [OPTION] [SEED]
  ; Simulate Conway's Game of Life.
  ; OPTION:
  ;     -n size of universe
  ; SEED:
  ;     blinker
  ;     beacon
  ;     toad
  ;     pinwheel
  ;     galaxy
  ;     glider
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args)
                  n (.get op "n") size 10 seed nil)
    (if (nil? args) (if n (<- size (int n)))
        (begin
          (if (nil? (<- seed ({} $seeds (symbol (car args)))))
              (raise ArgumentError "unknown seed"))
          (<- size (int (math.sqrt (len seed))))))
    (next-generation (first-generation (matrix (list size size)) seed)
                     (matrix (list size size)))))
