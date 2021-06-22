; room to grow solver.

(import :matrix)

(<- $board nil
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
  (map (f (pi) (* pi -1)) p))

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
      (write-line))))

(function vector->symbol (p)
  (if (= p '(0 -2)) 'a
      (= p '(2 0)) 's
      (= p '(0 2)) 'd
      (= p '(-2 0)) 'w
      (assert nil)))

(function neighbor-coods (p)
  (map (f (q) (map + p q)) $vectors))

(function edge (node1 node2)
  (map (f (pi qi) (/ (+ pi qi) 2)) node1 node2))

(function copy (x)
  (map slice x))

(function solved? ()
  (every? (f (p) (in? p (concat $goal-vertexes $goal-coods)))
          (concat $cuctus-vertexes $cuctus-coods)))

(macro restore (state)
  `(<- ($result $player-vertexes $cuctus-vertexes $cuctus-coods) ,state))

(function step (dir)
  (let (head (car $player-vertexes) next (map + head dir)
             inside-and-not-wall? (f (p) (&& (.inside? $board p) (!== (.at $board p) :wall)))
             state (map copy (list $result $player-vertexes $cuctus-vertexes $cuctus-coods)))
    (push! dir $result)
    (if (! (.inside? $board next)) (return (restore state)))
    (if (in? next $player-vertexes) (return (restore state)))
    (if (== (.at $board (edge head next)) :wall)
        (begin    ; rebound
          (<- dir (negate dir)
              $player-vertexes (cons head
                                     (cons (edge head (map + head dir))
                                           (map (f (p) (map + p dir)) $player-vertexes))))
          (if (! (every? inside-and-not-wall? $player-vertexes)) (return (restore state)))
          (<- unit-dir (map (f (x) (// x 2)) dir)
              $cuctus-coods (map (f (p) (if (in? (map + p unit-dir) $player-vertexes) (map + p dir) p))
                                 $cuctus-coods))
          (if (! (every? inside-and-not-wall? $cuctus-coods)) (return (restore state))))
        (begin    ; forward
          (<- $player-vertexes (cons next
                                     (cons (edge head next)
                                           $player-vertexes)))))
    (if (solved?)
        (begin
          (write (map vector->symbol (reverse! $result)))
          (quit))
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
  (let ((:key shape player-vertex cuctus-vertexes goal-vertexes cuctus-coods goal-coods wall-coods) expr)
    (<- $board (matrix (transform-coods shape))
        $player-vertexes (list (transform-vertex player-vertex))
        $cuctus-vertexes (map transform-vertex cuctus-vertexes)
        $goal-vertexes (map transform-vertex goal-vertexes)
        $cuctus-coods (map transform-coods cuctus-coods)
        $goal-coods (map transform-coods goal-coods)
        wall-coods (map transform-coods wall-coods))
    (dolist (p wall-coods)
      (when (.inside? $board p)
        (.put $board p :wall)
        (dolist (q (neighbor-coods p))
          (if (in? q wall-coods)
              (let (r (edge p q))
                (if (.inside? $board r) (.put $board r :wall)))))))
    (domatrix (p $board) (if (&& (nil? (.at $board p)) (! (coods? p))) (.put $board p :path)))))

(function! main (args)
  ; room-to-graw-solver FILE
  ; FILE -- map data.
  ; 
  ;    +-+-+-+-+-+
  ;    |G| | | | |
  ;    +-+-+-+-+-+
  ;    ##|G|C|C|## 
  ;    +#+-P-+-+#+
  ;
  ;    (:shape (2 5)
  ;     :player-vertex (2 2)
  ;     :goal-coods ((0 0) (1 1))
  ;     :cuctus-coods ((1 2) (1 3))
  ;     :wall-coods ((1 0) (1 4)
  ;                   (1 -1) (2 0) (1 5) (2 4)))   ;; for edges
  ;
  ;    -> (d w d d s a s d d)
  ;
  ;    +-+-+-+-+-+-+-+-+
  ;    | | | | | | |####
  ;    +-+-+-+-+-+-+#+-+
  ;    | | | | | | |#| |
  ;    +-+-+-+-+-+-+#+-+
  ;    | | |G| |C| |#| |
  ;    +-+-+-+-+-+-+#+-+
  ;    | | | | | | |####
  ;    +-+-+-P-+-+-+-+-+
  ;
  ;    (:shape (4 8)
  ;     :player-vertex (4 3)
  ;     :goal-coods ((2 2))
  ;     :cuctus-coods ((2 4))
  ;     :wall-coods ((0 6) (0 7) (0 8)
  ;                  (1 6)
  ;                  (2 6)
  ;                  (3 6) (3 7) (3 8)))
  ;        -> wddwddd
  ;
  (if (nil? args) (raise ArgumentError)
      (init (load-map (car args))))
  (show)
  (foreach step $vectors))
