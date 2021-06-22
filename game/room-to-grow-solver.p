; room to grow solver.

(import :matrix)
(import :optparse)

(<- $show-all? nil
    $debug? nil
    $board nil
    $result nil
    $player-vertexes nil
    $cuctus-vertexes nil
    $cuctus-coods nil
    $vectors '((0 -2) (2 0) (0 2) (-2 0)))

(function even? (x)
  (= (% x 2) 0))

(function coods? (p)
  (none? even? p))

(function vertex? (p)
  (every? even? p))

(function negate (p)
  (map (f (x) (* x -1)) p))

(function show ()
  (let ((x y) (.shape $board))
    (dotimes (i x)
      (dotimes (j y)
        (let (p (list i j) val (.at $board p))
          (write-bytes (if (== val :wall) "#"
                           (in? p $player-vertexes) "P"
                           (in? p (concat $cuctus-vertexes $cuctus-coods)) "C"
                           (in? p (concat $goal-vertexes $goal-coods)) "G"
                           (== val :path) (if (vertex? p) "+" (even? i) "-" "|")
                           " "))))
      (write-line))
    (write-line)))

(function vector->symbol (p)
  (if (= p '(0 -2)) 'a
      (= p '(2 0)) 's
      (= p '(0 2)) 'd
      (= p '(-2 0)) 'w
      (assert nil)))

(function neighbor-coods (p)
  (map (f (q) (map + p q)) $vectors))

(function midpoint (p q)
  (map (f (pi qi) (/ (+ pi qi) 2)) p q))

(function copy (x)
  (map slice x))

(function solved? ()
  (every? (f (p) (in? p (concat $goal-vertexes $goal-coods)))
          (concat $cuctus-vertexes $cuctus-coods)))

(macro restore (state)
  `(<- ($result $player-vertexes $cuctus-vertexes $cuctus-coods) ,state))

(function step (dir)
  (let (head (car $player-vertexes) next (map + head dir) mid (midpoint head next)
             inside-and-not-wall? (f (p) (&& (.inside? $board p) (!== (.at $board p) :wall)))
             state (map copy (list $result $player-vertexes $cuctus-vertexes $cuctus-coods)))
    (push! dir $result)
    (if (! (.inside? $board next)) (return (restore state))
        (== (.at $board mid) :stop) (return (restore state))
        (== (.at $board mid) :wall)
        ;; opposite
        (let (dir (negate dir) unit-dir (midpoint dir '(0 0)))
          (<- $player-vertexes (cons head
                                     (cons (map + head unit-dir)
                                           (map (f (p) (map + p dir)) $player-vertexes)))
              $cuctus-coods (map (f (p) (if (in? (map + p unit-dir) $player-vertexes) (map + p dir) p))
                                 $cuctus-coods))
          (if (! (every? inside-and-not-wall? (concat $player-vertexes $cuctus-coods))) (return (restore state))))
        ;; forward
        (begin
          (if (in? next $player-vertexes) (return (restore state))
              (<- $player-vertexes (cons next (cons mid $player-vertexes))))))
    (if $debug? (show))
    (if (solved?)
        (begin
          (write (map vector->symbol (reverse $result)))
          (if (! $show-all?) (quit)))
        (begin
          (dolist (dir $vectors) (step dir))
          (restore state)))))

(function load-map (file-name)
  (with-open ($in file-name :read)
    (return (read))))

(function transform-coods (p)
  (map (f (x) (++ (* x 2))) p))

(function transform-vertex (p)
  (map (f (x) (* x 2)) p))

(function init (expr)
  (let ((:key shape
              player-vertex cuctus-vertexes goal-vertexes
              cuctus-coods goal-coods wall-coods
              stop-edges) expr)
    (<- $board (matrix (transform-coods shape))
        $player-vertexes (list (transform-vertex player-vertex))
        $cuctus-vertexes (map transform-vertex cuctus-vertexes)
        $goal-vertexes (map transform-vertex goal-vertexes)
        $cuctus-coods (map transform-coods cuctus-coods)
        $goal-coods (map transform-coods goal-coods)
        wall-coods (map transform-coods wall-coods))
    (dolist (edge stop-edges)
      (let ((p q) (map transform-vertex edge))
        (if (&& (.inside? $board p) (.inside? $board q))
            (.put $board (midpoint p q) :stop))))
    (dolist (p wall-coods)
      (when (.inside? $board p)
        (.put $board p :wall)
        (dolist (q (neighbor-coods p))
          (if (in? q wall-coods)
              (let (r (midpoint p q))
                (if (.inside? $board r) (.put $board r :wall)))))))
    (domatrix (p $board) (if (&& (nil? (.at $board p)) (! (coods? p))) (.put $board p :path)))))

(function! main (args)
  ; room-to-graw-solver FILE
  ; Read the game problem from FILE and display the solution.
  ;     -d debug mode
  ;     -a show all solution
  ; The FILE is described by an S-expression.
  ;     :shape -- The shape of the problem.
  ;     :player-vertex -- Player initial coordinates. ((0, 0) <= x < shape)
  ;     :goal-coods -- Goal coordinates. ((0, 0) <= x < shape)
  ;     :cuctus-coods -- Cuctus initial coordinates. ((0, 0) <= x < shape)
  ;     :wall-coods -- Wall coordinates. ((0, 0) <= x < shape)
  ;     :stop-edges -- Line segments that cannot pass.
  ;
  ; Example of file contents.
  ;
  ;     (:shape (2 5)
  ;      :player-vertex (2 2)
  ;      :goal-coods ((0 0) (1 1))
  ;      :cuctus-coods ((1 2) (1 3))
  ;      :wall-coods ((1 0) (1 4)
  ;                   (1 -1) (2 0) (1 5) (2 4)))
  ;     +-+-+-+-+-+
  ;     |G| | | | |
  ;     +-+-+-+-+-+
  ;     ##|G|C|C|## 
  ;     +#+-P-+-+#+
  ;
  ;     -> (d w d d s a s d d)
  ;
  ;     (:shape (5 7)
  ;      :player-vertex (3 2)
  ;      :goal-coods ((1 2))
  ;      :cuctus-coods ((1 3))
  ;      :stop-edges (((1 4) (2 4)))
  ;      :wall-coods ((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (0 6)
  ;                   (1 0) (1 1)                         (1 6)
  ;                   (2 0) (2 1)                         (2 6)
  ;                   (3 0)                               (3 6)
  ;                   (4 0) (4 1) (4 2) (4 3) (4 4) (4 5) (4 6)))
  ;
  ;     +-+-+-+-+-+-+-+
  ;     |#############|
  ;     +#+#+-+-+-+-+#+
  ;     |###|G|C  | |#|
  ;     +#+#+-+-+-+-+#+
  ;     |###| | | | |#|
  ;     +#+-P-+-+-+-+#+
  ;     |#| | | | | |#|
  ;     +#+-+-+-+-+-+#+
  ;     |#############|
  ;     +-+-+-+-+-+-+-+
  ;
  ;     -> (d d d d w a w a w d d s d)
  ;
  ;    (:shape (3 7)
  ;     :player-vertex (3 2)
  ;     :goal-coods ((1 1))
  ;     :cuctus-coods ((1 3))
  ;     :wall-coods ((0 5) (0 6) (0 7)
  ;                  (1 5)
  ;                  (2 5) (2 6) (2 7)))
  ;
  ;    +-+-+-+-+-+-+-+
  ;    | | | | | |####
  ;    +-+-+-+-+-+#+-+
  ;    | |G| |C| |#| |
  ;    +-+-+-+-+-+#+-+
  ;    | | | | | |####
  ;    +-+-P-+-+-+-+-+
  ;
  ;    -> (d d d w a w a a w d d d s d s d)
  (let ((op args) (.parse (.init (.new OptionParser) "ad") args))
    (if (nil? args) (raise ArgumentError)
        (<- $show-all? (.get op "a")
            $debug? (.get op "d")))
    (init (load-map (car args)))
    (show)
    (foreach step $vectors)))
