; room to grow solver.

(import :matrix)
(import :optparse)

(<- $show-all? nil    ;; option -a
    $debug? nil    ;; option -d
    $board nil
    $state '($result $head-vertexes $body-vertexes $cuctus-vertexes $cuctus-coods)
    $unit-vectors '((0 -1) (1 0) (0 1) (-1 0))
    $vectors '((0 -2) (2 0) (0 2) (-2 0)))

(function even? (x)
  (= (% x 2) 0))

(function coods? (p)
  (none? even? p))

(function vertex? (p)
  (every? even? p))

(function negate (p)
  (map (f (x) (* x -1)) p))

(function rotate (l)
  (if l (concat (cdr l) (list (car l)))))

(macro make-state ()
  (cons list $state))

(macro with-state ((state) :rest body)
  (with-gensyms (gstate)
    `(let (,gstate ,state ,$state ,gstate) ,@body)))

(function vector->symbol (p)
  (if (= p '(0 -2)) 'a
      (= p '(2 0)) 's
      (= p '(0 2)) 'd
      (= p '(-2 0)) 'w
      (assert nil)))

(function symbol->vector (x)
  (if (== x 'a) '(0 -2)
      (== x 's) '(2 0)
      (== x 'd) '(0 2)
      (== x 'w) '(-2 0)
      (== x 'x) '(0 0)
      (assert nil)))

(function neighbor-coods (p)
  (map (f (q) (map + p q)) $vectors))

(function neighbor (p)
  (map (f (q) (map + p q)) $unit-vectors))

(function midpoint (p q)
  (map (f (pi qi) (/ (+ pi qi) 2)) p q))

(function collided? (P :opt Q)
  (let (collided? (f (p P)
                    (if (nil? p) nil
                        (|| (! (.inside? $board p))
                            (== (.at $board p) :wall)
                            (in? p P)
                            (&& Q (in? p Q))
                            (collided? (car P) (cdr P))))))
    (collided? (car P) (cdr P))))

(macro solved? ()
  ;; required state context.
  `(&& (every? (f (p) (in? p $goal-vertexes)) $cuctus-vertexes)
       (every? (f (p) (in? p $goal-coods)) $cuctus-coods)))

(macro show ()
  ;; required state context.
  `(let ((x y) (.shape $board))
     (dotimes (i x)
       (dotimes (j y)
         (let (p (list i j) val (.at $board p))
           (write-bytes (if (== val :wall) "#"
                            (in? p $head-vertexes) "@"
                            (in? p $body-vertexes) "*"
                            (|| (in? p $cuctus-vertexes) (in? p $cuctus-coods)) "C"
                            (|| (in? p $goal-vertexes) (in? p $goal-coods)) "G"
                            (== val :thick-path) "%"
                            (== val :path) (if (vertex? p) "+" (even? i) "-" "|")
                            " "))))
       (write-line))
     (write-line)))

(macro move-forward ()
  ;; required state context.
  `(let (unit-dir (midpoint dir '(0 0)))
     (if (== (.at $board next-head) :thick-path)
         (let ((heads bodies) (neighbor-tchick next-head))
           (<- $head-vertexes heads
               $body-vertexes (concat $body-vertexes
                                      bodies
                                      (list head (map + head unit-dir)))))
         (<- $head-vertexes (map (f (p) (if (= p head) next-head p))
                                 $head-vertexes)
             $body-vertexes (cons head
                                  (cons (map + head unit-dir)
                                        $body-vertexes))))
     (<- $cuctus-vertexes (map (f (p) (if (= p next-head) (map + p dir) p))
                               $cuctus-vertexes))
     (let (P (concat $head-vertexes $body-vertexes))
       (if (|| (collided? P) (collided? $cuctus-vertexes P)) (return nil)))))

(function neighbor-tchick (p)
  (let (collect (f (p acc)
                  (let (Q (select (f (q) (== (.at $board q) :thick-path))
                                  (except (f (q) (in? q acc))
                                          (neighbor p))))
                    (<- acc (concat acc Q))
                    (dolist (q Q)
                      (<- acc (collect q acc)))
                    acc))
                divide (f (Q)
                         (let (head nil body nil)
                           (dolist (q Q)
                             (let (neighbor (neighbor q))
                               (if (&& (!= p q)
                                       (= (len (select (f (r) (in? r neighbor)) Q)) 1))
                                   (push! q head)
                                   (push! q body))))
                           (list head body))))
    (divide (collect p nil))))

(macro move-opposite ()
  ;; required state context.
  `(let (dir (negate dir) unit-dir (midpoint dir '(0 0)))
     (<- $head-vertexes (map (f (p) (if (= p head) head (map + p dir)))
                             $head-vertexes)
         $body-vertexes (cons (map + head dir)
                              (cons (map + head unit-dir)
                                    (map (f (p) (map + p dir)) $body-vertexes)))
         $cuctus-coods (map (f (p) (if (in? (map + p unit-dir) $body-vertexes) (map + p dir) p))
                            $cuctus-coods))
     (if (|| (collided? $head-vertexes)    ; for other head.
             (collided? $body-vertexes)
             (collided? $cuctus-coods))
         (return nil))))

(function step (dir state :opt hint)
  (with-state (state)
    (if (= dir '(0 0))
        (begin
          ;; hint 'x
          (<- $head-vertexes (rotate $head-vertexes))
          (push! 'x $result)
          (step (car hint) (make-state) (cdr hint)))
        (dolist (head $head-vertexes)
          (let (next-head (map + head dir))
            (if (.inside? $board next-head)
                (let (v (.at $board (midpoint head next-head)))
                  (if (== v :wall) (move-opposite)
                      (!== v :stop) (move-forward)
                      (return nil))
                  (push! (vector->symbol dir) $result)
                  (if $debug? (show))
                  (if (solved?)
                      (begin
                        (write (reverse $result))
                        (if (! $show-all?) (quit)))
                      (begin
                        (if (nil? hint) (dolist (dir $vectors) (step dir (make-state)))
                            (step (car hint) (make-state) (cdr hint)))))))
            (<- $head-vertexes (rotate $head-vertexes))
            (push! 'x $result))))))

(function load-map (file-name)
  (with-open ($in file-name :read)
    (return (read))))

(function transform-coods (p)
  (map (f (x) (++ (* x 2))) p))

(function transform-vertex (p)
  (map (f (x) (* x 2)) p))

(function init-edges (edges type)
  (dolist (edge edges)
    (let ((p q) (map transform-vertex edge))
      (when (== type :thick-path)
        (.put $board p type) (.put $board q type))
      (.put $board (midpoint p q) type))))

(function init (expr)
  (let ((:key shape player-vertex
              cuctus-vertexes goal-vertexes
              cuctus-coods goal-coods
              wall-coods
              stop-edges thick-edges) expr)
    (<- $board (matrix (transform-coods shape))
        $result nil
        $head-vertexes (list (transform-vertex player-vertex))
        $body-vertexes nil
        $goal-vertexes (map transform-vertex goal-vertexes)
        $goal-coods (map transform-coods goal-coods)
        $cuctus-vertexes (map transform-vertex cuctus-vertexes)
        $cuctus-coods (map transform-coods cuctus-coods)
        wall-coods (map transform-coods wall-coods))
    (init-edges stop-edges :stop)
    (init-edges thick-edges :thick-path)
    (dolist (p wall-coods)
      (when (.inside? $board p)
        (.put $board p :wall)
        (dolist (q (neighbor-coods p))
          (if (in? q wall-coods)
              (let (r (midpoint p q))
                (if (.inside? $board r) (.put $board r :wall)))))))
    (domatrix (p $board)
      (if (&& (nil? (.at $board p)) (! (coods? p))) (.put $board p :path)))
    (make-state)))

(function! main (args)
  ; room-to-graw-solver FILE [HINT]
  ; Read the game problem from FILE and display the solution.
  ;     -d debug mode
  ;     -a show all solution
  ;
  ; The FILE is described by an S-expression.
  ; vertex represents the position (starting from 0) on the line.
  ; cood represents the position (starting from 0) of the square.
  ;     :shape -- The shape of the problem.
  ;     :player-vertex -- Player position.
  ;     :goal-vertexes -- Goal positions.
  ;     :cuctus-vertexes -- Cuctus positions.
  ;     :goal-coods -- Goal positions.
  ;     :cuctus-coods -- Cuctus positions.
  ;     :wall-coods -- Wall position.
  ;     :stop-edges -- Line segments that cannot pass.
  ;
  ; If HINT is specified, perform the first step according to the hint.
  ; Therefore, if the solution is known, the transition of the state from the beginning to the end can be confirmed by specifying the solution in HINT.
  ;
  ; Example of file contents.
  ;
  ;     +-+-+-+-+-+
  ;     |G| | | | |
  ;     +-+-+-+-+-+
  ;     ##|G|C|C|##
  ;     +#+-@-+-+#+
  ;
  ;     (:shape (2 5)
  ;      :player-vertex (2 2)
  ;      :goal-coods ((0 0) (1 1))
  ;      :cuctus-coods ((1 2) (1 3))
  ;      :wall-coods ((1 0) (1 4)
  ;                   (1 -1) (2 0) (1 5) (2 4)))
  ;
  ;     +-+-+-+-+-+-+-+
  ;     |#############|
  ;     +#+#+-+-+-+-+#+
  ;     |###|G|C  | |#|
  ;     +#+#+-+-+-+-+#+
  ;     |###| | | | |#|
  ;     +#+-@-+-+-+-+#+
  ;     |#| | | | | |#|
  ;     +#+-+-+-+-+-+#+
  ;     |#############|
  ;     +-+-+-+-+-+-+-+
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
  ;    +-+-+-+-+-+-+-+
  ;    | | | | | |####
  ;    +-+-+-+-+-+#+-+
  ;    | |G| |C| |#| |
  ;    +-+-+-+-+-+#+-+
  ;    | | | | | |####
  ;    +-+-@-+-+-+-+-+
  ;
  ;    (:shape (3 7)
  ;     :player-vertex (3 2)
  ;     :goal-coods ((1 1))
  ;     :cuctus-coods ((1 3))
  ;     :wall-coods ((0 5) (0 6) (0 7)
  ;                  (1 5)
  ;                  (2 5) (2 6) (2 7)))
  ;     +-G-+-G-+
  ;     | | | | |
  ;     +-C-+-C-+
  ;     | | | | |
  ;     +-%%%%%-+
  ;     | | | | |
  ;     +-+-@-+-+
  ;
  ;     (:shape (3 4)
  ;      :player-vertex (3 2)
  ;      :goal-vertexes ((0 1) (0 3))
  ;      :cuctus-vertexes ((1 1) (1 3))
  ;      :thick-edges (((2 1) (2 2)) ((2 2) (2 3))))
  ;
  ; Example of usage.
  ;
  ; $ cat args.p
  ;
  ; (:shape (6 7)
  ;  :player-vertex (2 2)
  ;  :goal-coods ((3 1) (3 5))
  ;  :cuctus-coods ((1 1) (2 2))
  ;  :wall-coods ((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (0 6)
  ;               (1 0)                               (1 6)
  ;               (2 0)                               (2 6)
  ;               (3 0)       (3 2) (3 3) (3 4)       (3 6)
  ;               (4 0) (4 1) (4 2)       (4 4)       (4 6)
  ;                                       (5 4) (5 5) (5 6)))
  ;
  ; $ paren room-to-grow-solver.p args.p
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#+-+-+-+-+-+#+
  ; |#|C| | | | |#|
  ; +#+-@-+-+-+-+#+
  ; |#| |C| | | |#|
  ; +#+-+-+-+-+-+#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; (s d w w a a a a a w d d d d w)
  ;
  ; $ paren room-to-grow-solver.p -d args.p sdwwaaaaww
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#+-+-+-+-+-+#+
  ; |#|C| | | | |#|
  ; +#+-@-+-+-+-+#+
  ; |#| |C| | | |#|
  ; +#+-+-+-+-+-+#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#+-+-+-+-+-+#+
  ; |#|C| | | | |#|
  ; +#+-*-+-+-+-+#+
  ; |#| *C| | | |#|
  ; +#+-@-+-+-+-+#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#+-+-+-+-+-+#+
  ; |#|C| | | | |#|
  ; +#+-*-+-+-+-+#+
  ; |#| *C| | | |#|
  ; +#+-**@-+-+-+#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#+-+-+-+-+-+#+
  ; |#|C| | | | |#|
  ; +#+-*-@-+-+-+#+
  ; |#| *C* | | |#|
  ; +#+-***-+-+-+#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#+-+-@-+-+-+#+
  ; |#|C| * | | |#|
  ; +#+-*-*-+-+-+#+
  ; |#| *C* | | |#|
  ; +#+-***-+-+-+#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#+-@**-+-+-+#+
  ; |#|C| * | | |#|
  ; +#+-*-*-+-+-+#+
  ; |#| *C* | | |#|
  ; +#+-***-+-+-+#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#@****-+-+-+#+
  ; |#|C| * | | |#|
  ; +#+-*-*-+-+-+#+
  ; |#| *C* | | |#|
  ; +#+-***-+-+-+#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#@******-+-+#+
  ; |#|C| | * | |#|
  ; +#+-+-*-*-+-+#+
  ; |#| | *C* | |#|
  ; +#+-+-***-+-+#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#@********-+#+
  ; |#|C| | | * |#|
  ; +#+-+-+-*-*-+#+
  ; |#| | | *C* |#|
  ; +#+-+-+-***-+#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#@**********#+
  ; |#|C| | | | *#|
  ; +#+-+-+-+-*-*#+
  ; |#| | | | *C*#|
  ; +#+-+-+-+-***#+
  ; |#|G|#####|G|#|
  ; +#+-+#+-+#+-+#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#@-+-+-+-+-+#+
  ; |#* | | | | |#|
  ; +#***********#+
  ; |#|C| | | |C*#|
  ; +#+-+-+-+-*-*#+
  ; |#|G|#####*G*#|
  ; +#+-+#+-+#***#+
  ; |#####| |#| |#|
  ; +-+-+-+-+#+-+#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; +-+-+-+-+-+-+-+
  ; |#############|
  ; +#@-+-+-+-+-+#+
  ; |#* | | | | |#|
  ; +#*-+-+-+-+-+#+
  ; |#* | | | | |#|
  ; +#***********#+
  ; |#|C|#####|C*#|
  ; +#+-+#+-+#*-*#+
  ; |#####| |#* *#|
  ; +-+-+-+-+#***#+
  ; | | | | |#####|
  ; +-+-+-+-+-+-+-+
  ;
  ; (s d w w a a a a a w w)
  ;
  (let ((op args) (.parse (.init (.new OptionParser) "ad") args)
                  file (car args) hint (map symbol->vector (map symbol (split (cadr args)))))
    (if (nil? file) (raise ArgumentError)
        (<- $show-all? (.get op "a")
            $debug? (.get op "d")))
    (let (state (init (load-map file)))
      (show)
      (if (nil? hint) (dolist (dir $vectors) (step dir state))
          (step (car hint) state (cdr hint))))))
