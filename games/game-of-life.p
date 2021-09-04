; Conway's Game of Life.

(import :console)
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
  (len (reject nil? (map (f (q)
                           (let (r (map + p q))
                             (&& (.inside? universe r) (.at universe r))))
                         '((-1 -1) (-1  0) (-1  1)
                           ( 0 -1)         ( 0  1)
                           ( 1 -1) ( 1  0) ( 1  1))))))

(function first-generation (universe initial-value)
  (let ((rows cols) (.shape universe))
    (dotimes (x rows)
      (dotimes (y cols)
        (.put universe (list x y) (= (car initial-value) 1))
        (<- initial-value (cdr initial-value))))
    universe))

(function next-generation (curr next)
  (show curr)
  (domatrix (p curr)
    (let (n (neighbor-count curr p))
      (.put next p (|| (&& (.at curr p) (= n 2)) (= n 3)))))
  (next-generation next curr))

(function load-initial-value (pattern)
  (let (val ([] $seeds (symbol pattern)))
    (if (nil? val) (raise ArgumentError "unknown pattern")
        val)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args)
                  size (int (.get op "n" 10)) initial-value nil)
    (if (nil? args) (<- initial-value (map (f (x) (rand.int 2)) (.. (* size size))))
        (<- initial-value (load-initial-value (car args))
            size (int (sqrt (len initial-value)))))
    (next-generation (first-generation (matrix (list size size)) initial-value)
                     (matrix (list size size)))))
