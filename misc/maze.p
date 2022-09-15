; maze.

(import :rand)
(import :matrix)

(<- $width 20 $height 40
    $wall "#" $path " "
    $map (matrix (list (++ $width) (++ $height)))
    $direction '((0 -1) (1 0) (-1 0) (0 1)))

(function show ()
  (let ((width height) (.shape $map))
    (dotimes (x width)
      (dotimes (y height)
        (if (.at $map (list x y)) (print $path)
            (print $wall)))
      (println))))

(function generate-maze (p)
  ;; Generate a maze, using the simple Depth-first search algorithm.
  (.put $map p true)
  (foreach (f (q)
             (let (p+q (map + p q) p+2q (map + p+q q))
               (when (&& (.inside? $map p+2q) (nil? (.at $map p+2q)))
                 (.put $map p+q  true)
                 (generate-maze p+2q))))
           (rand.shuffle! $direction)))

(function! main (args)
  (rand.seed (time))
  (generate-maze (list (rand.choice (.. 1 $width 2)) (rand.choice (.. 1 $height 2))))
  (show))
