; room to grow solver.

(import :matrix)
(import :optparse)

(<- $show-all? nil    ;; option -a
    $debug? nil    ;; option -d
    $board nil
    $hint nil
    $state '($result $head-vertexes $body-vertexes $cuctus-vertexes $cuctus-coods $branches)
    $unit-vectors '((0 -1) (1 0) (0 1) (-1 0)))

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
  (if (= p '(0 -1)) 'a
      (= p '(1 0)) 's
      (= p '(0 1)) 'd
      (= p '(-1 0)) 'w
      (= p '(0 0)) 'x
      (assert nil)))

(function symbol->vector (x)
  (if (== x 'a) '(0 -1)
      (== x 's) '(1 0)
      (== x 'd) '(0 1)
      (== x 'w) '(-1 0)
      (== x 'x) '(0 0)
      (assert nil)))

(function neighbor (p)
  (map (f (q) (map + p q)) $unit-vectors))

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
                            (in? p $body-vertexes) "o"
                            (|| (in? p $cuctus-vertexes) (in? p $cuctus-coods)) "C"
                            (|| (in? p $goal-vertexes) (in? p $goal-coods)) "G"
                            (in? p $branches) "%"
                            (== val :path) (if (vertex? p) "+" (even? i) "-" "|")
                            " "))))
       (write-line))
     (write-line)))

(class BackTrackSignal (Error))

(macro head-and-body-vertexes ()
  '(concat $head-vertexes $body-vertexes))

(macro placeble? (q)
  `(&& (.inside? $board ,q) (!== (.at $board ,q) :wall) (!== (.at $board ,q) :no-reaction-wall)))

(macro move-forward ()
  ;; required state context.
  `(<- ($head-vertexes $body-vertexes $cuctus-vertexes $branches)
       (move-forward1 dir $head (cdr $head-vertexes) $body-vertexes $cuctus-vertexes $branches)))

(function move-forward1 (dir curr-head rest-head body-vertexes cuctus-vertexes branches)
  (let (dz (map + dir dir) next-head (map + dz curr-head))
    (if (|| (! (placeble? next-head))
            (in? next-head rest-head)
            (in? next-head body-vertexes)) (raise BackTrackSignal))
    (<- body-vertexes (cons curr-head (cons (map + curr-head dir) body-vertexes))
        cuctus-vertexes (map (f (p)
                               (if (!= p next-head) p
                                   (let (q (map + p dz))
                                     (if (&& (placeble? q)
                                             (! (in? q rest-head))
                                             (! (in? q body-vertexes))
                                             (! (in? q cuctus-vertexes))) q
                                         (raise BackTrackSignal)))))
                             cuctus-vertexes)
        head-vertexes (cons next-head rest-head)
        curr-head next-head)
    (dolist (branch-dir $unit-vectors)
      (let (p (map + branch-dir curr-head))
        (if (in? p branches)
            (<- (head-vertexes body-vertexes cuctus-vertexes branches)
                (move-forward1 branch-dir curr-head (reject (f (p) (= p curr-head)) head-vertexes)
                               body-vertexes cuctus-vertexes (reject (f (q) (= p q)) branches))))))
    (list head-vertexes body-vertexes cuctus-vertexes branches)))

(macro move-opposite ()
  ;; required state context.
  `(let (dir (negate dir) dz (map + dir dir))
     (<- $head-vertexes (map (f (p)
                               (if (= p $head) $head
                                   (let (q (map + p dz))
                                     (if (placeble? q) q
                                         (raise BackTrackSignal)))))
                             $head-vertexes)
         $body-vertexes (cons (map + $head dir)
                              (cons (map + $head dz)
                                    (map (f (p)
                                           (let (q (map + p dz))
                                             (if (placeble? q) q
                                                 (raise BackTrackSignal))))
                                         $body-vertexes)))
         $cuctus-coods (map (f (p)
                              (if (! (in? (map + p dir) (head-and-body-vertexes))) p
                                  (let (q (map + p dz))
                                    (if (&& (placeble? q) (! (in? q $cuctus-coods))) q
                                        (raise BackTrackSignal)))))
                            $cuctus-coods))))

(function step1 (dir state)
  (with-state (state)
    (push! dir $result)
    (catch (BackTrackSignal (f (e) (if $debug? (write :back-track)) (<- $hint nil)))
      (let ($head (car $head-vertexes) peek (map + $head dir))
        (if (! (.inside? $board peek)) (raise BackTrackSignal))
        (if (== (.at $board peek) :wall) (move-opposite)
            (== (.at $board peek) :path) (move-forward)
            (raise BackTrackSignal))
        (if $debug? (write (vector->symbol dir)))
        (if (|| $debug? $hint) (show))
        (if (solved?)
            (begin
              (show)
              (write (reverse (map vector->symbol $result)))
              (if (! $show-all?) (quit)))
            (step (make-state)))))))

(function step-dir (state)
  (dolist (dir $unit-vectors) (step1 dir state)))

(macro switch-head ()
  ;; required state context.
  `(begin
     (if $debug? (write :swich))
     (push! (symbol->vector 'x) $result)
     (<- $head-vertexes (rotate $head-vertexes))))

(function step (state)
  (with-state (state)
    (if (nil? $hint)
        (begin
          (step-dir (make-state))
          (dotimes (_ (-- (len $head-vertexes)))
            (switch-head)
            (step-dir (make-state))))
        (let (hint (car $hint))
          (<- $hint (cdr $hint))
          (if (!== hint 'x) (step1 (symbol->vector hint) (make-state))
              (begin
                (switch-head)
                (step (make-state))))))))

(function load-map (file-name)
  (with-open ($in file-name :read)
    (return (read))))

(function transform-coods (p)
  (map (f (x) (++ (* x 2))) p))

(function transform-vertex (p)
  (map (f (x) (* x 2)) p))

(function init-edges (edges type)
  (dolist (edge edges)
    ;; p(p1, p2) q(q1, q2)
    (let ((p q) (map transform-vertex edge) (p1 p2) p (q1 q2) q)
      (for (x1 p1) (<= x1 q1) (x1 (++ x1))
        (for (x2 p2) (<= x2 q2) (x2 (++ x2))
          (let (r (list x1 x2))
            (if (! (vertex? r)) (.put $board r type))))))))

(function init (expr)
  (let ((:key shape player-vertex
              cuctus-vertexes goal-vertexes
              cuctus-coods goal-coods
              wall-edges no-reaction-wall-edges stop-edges thick-edges) expr)
    (<- $board (matrix (transform-coods shape))
        $result nil
        $head-vertexes (list (transform-vertex player-vertex))
        $body-vertexes nil
        $goal-vertexes (map transform-vertex goal-vertexes)
        $goal-coods (map transform-coods goal-coods)
        $cuctus-vertexes (map transform-vertex cuctus-vertexes)
        $cuctus-coods (map transform-coods cuctus-coods)
        $branches nil)
    (init-edges stop-edges :stop)
    (init-edges thick-edges :path)
    (init-edges wall-edges :wall)
    (init-edges no-reaction-wall-edges :no-reaction-wall)
    (domatrix (p $board)
      (if (! (coods? p))
          (let (v (.at $board p))
            (if (nil? v) (.put $board p :path)
                (== v :path) (push! p $branches)))))
    (make-state)))

(function! main (args)
  ; room-to-graw-solver FILE [HINT]
  ; Read the game problem from FILE and display the solution.
  ;     -d debug mode
  ;     -a show all solution
  ;
  ; The FILE is described by the S-expression.
  ; vertex represents the position (starting from 0) on the line.
  ; cood represents the position (starting from 0) of the square.
  ; edge represents the line segment consisting of two vertices.
  ;     :shape -- The shape of the problem.
  ;     :player-vertex -- Player position.
  ;     :goal-vertexes -- Goal positions.
  ;     :cuctus-vertexes -- Cuctus positions.
  ;     :goal-coods -- Goal positions.
  ;     :cuctus-coods -- Cuctus positions.
  ;     :wall-edges -- Line segment representing the wall.
  ;     :stop-edges -- Line segments that cannot pass.
  ;
  ; If HINT is specified, perform the first step according to the hint.
  ; Therefore, if the solution is known, the transition of the state from the beginning to the end can be confirmed by specifying the solution in HINT.
  ;
  ; The following functions are not supported.
  ; - desert area rules
  ;     - oblique coordinates
  ;     - coordinate with a mixture of different widths
  ;
  ; Example of the S-expression.
  ;
  ;     +-+-+-+-+-+-+-+
  ;     | | | | | | # #
  ;     +-+-+-+-+-+#+#+
  ;     | |G| |C| | # #
  ;     +-+-+-+-+-+#+#+
  ;     | | | | | | # #
  ;     +-+-@-+-+-+-+-+
  ;
  ; (:shape (3 7)
  ;  :player-vertex (3 2)
  ;  :goal-coods ((1 1))
  ;  :cuctus-coods ((1 3))
  ;  :wall-edges (((0 6) (3 6))
  ;               ((0 7) (3 7))
  ;               ((1 5) (1 7))
  ;               ((2 5) (2 7))))
  ;
  ;     +-G-+-G-+
  ;     | | | | |
  ;     +-C-+-C-+
  ;     | | | | |
  ;     +-+%+%+-+
  ;     | | | | |
  ;     +-+-@-+-+
  ;
  ; (:shape (3 4)
  ;  :player-vertex (3 2)
  ;  :goal-vertexes ((0 1) (0 3))
  ;  :cuctus-vertexes ((1 1) (1 3))
  ;  :thick-edges (((2 1) (2 2)) ((2 2) (2 3))))
  ;
  ;     +#+#+#+#+#+#+#+
  ;     # # # # # # # #
  ;     +#+#+-+-+-+-+#+
  ;     # # |G|C  | | #
  ;     +#+#+-+-+-+-+#+
  ;     # # | | | | | #
  ;     +#+-@-+-+-+-+#+
  ;     # | | | | | | #
  ;     +#+-+-+-+-+-+#+
  ;     # # # # # # # #
  ;     +#+#+#+#+#+#+#+
  ;
  ; (:shape (5 7)
  ;  :player-vertex (3 2)
  ;  :goal-coods ((1 2))
  ;  :cuctus-coods ((1 3))
  ;  :stop-edges (((1 4) (2 4)))
  ;  :wall-edges (((0 0) (0 7))
  ;               ((1 0) (1 2)) ((1 6) (1 7))
  ;               ((2 0) (2 2)) ((2 6) (2 7))
  ;               ((3 0) (3 1)) ((3 6) (3 7))
  ;               ((4 0) (4 1)) ((4 6) (4 7))
  ;               ((5 0) (5 7)) ((5 6) (5 7))
  ;               ((0 0) (5 0))
  ;               ((0 1) (3 1)) ((4 1) (5 1))
  ;               ((0 2) (1 2)) ((4 2) (5 2))
  ;               ((0 3) (1 3)) ((4 3) (5 3))
  ;               ((0 4) (1 4)) ((4 4) (5 4))
  ;               ((0 5) (1 5)) ((4 5) (5 5))
  ;               ((0 6) (1 6)) ((4 6) (5 6))
  ;               ((0 7) (5 7))))
  ;
  ;
  ; Example of usage.
  ;
  ; $ cat args.p
  ;
  ; (:shape (2 5)
  ;  :player-vertex (2 2)
  ;  :goal-coods ((0 0) (1 1))
  ;  :cuctus-coods ((1 2) (1 3))
  ;  :wall-edges (((1 0) (2 0)) ((2 0) (2 1))
  ;               ((1 5) (2 5)) ((2 4) (2 5))))
  ;
  ; $ paren room-to-grow-solver.p args.p
  ;
  ; +-+-+-+-+-+
  ; |G| | | | |
  ; +-+-+-+-+-+
  ; # |G|C|C| #
  ; +#+-@-+-+#+
  ;
  ; (d w d d s a s d d)
  ;
  ; $ paren room-to-grow-solver args.wk dwddsasdd
  ; +-+-+-+-+-+
  ; |G| | | | |
  ; +-+-+-+-+-+
  ; # |G|C|C| #
  ; +#+-@-+-+#+
  ;
  ; +-+-+-+-+-+
  ; |G| | | | |
  ; +-+-+-+-+-+
  ; # |G|C|C| #
  ; +#+-oo@-+#+
  ;
  ; +-+-+-+-+-+
  ; |G| | | | |
  ; +-+-+-@-+-+
  ; # |G|CoC| #
  ; +#+-ooo-+#+
  ;
  ; +-+-+-+-+-+
  ; |G| | | | |
  ; +-+-+-oo@-+
  ; # |G|CoC| #
  ; +#+-ooo-+#+
  ;
  ; +-+-+-+-+-+
  ; |G| | | | |
  ; +-+-+-oooo@
  ; # |G|CoC| #
  ; +#+-ooo-+#+
  ;
  ; +-+-+-ooooo
  ; |G| |Co | o
  ; +-+-ooo-+-@
  ; # |G| |C| #
  ; +#+-+-+-+#+
  ;
  ; +-+-+-ooooo
  ; |G| |Co | o
  ; +-+-ooo-@oo
  ; # |G| |C| #
  ; +#+-+-+-+#+
  ;
  ; +-+-+-ooooo
  ; |G| |Co | o
  ; +-+-ooo-ooo
  ; # |G| |Co #
  ; +#+-+-+-@#+
  ;
  ; +-+-ooooo-+
  ; |G|Co | o |
  ; +-ooo-ooo-+
  ; # |G|Co | #
  ; +#+-+-oo@#+
  ;
  ; +-ooooo-+-+
  ; |Co | o | |
  ; ooo-ooo-+-+
  ; # |Co | | #
  ; +#+-oooo@#+
  ;
  ; (d w d d s a s d d)
  ;
  (let ((op args) (.parse (.init (.new OptionParser) "ad") args) file (car args))
    (if (nil? file) (raise ArgumentError)
        (<- $show-all? (.get op "a")
            $debug? (.get op "d")
            $hint (map symbol (split (cadr args)))))
    (let (state (init (load-map file)))
      (show)
      (step state))))
