; maze.

(import :rand)
(import :matrix)

(<- $maze (matrix'(20 40))
    $direction '((0 -1) (1 0) (-1 0) (0 1))
    $wall "#"
    $path " ")

(function neighbors (p)
  (map (f (q) (map + q p)) (rand.shuffle! $direction)))

(function show ()
  (let ((rows cols) (.shape $maze))
    (dotimes (i (+ rows 2))
      (dotimes (j (+ cols 2))
        (if (|| (= i 0) (= j 0) (= i (++ rows)) (= j (++ cols))) (write-bytes $wall)
            (.at $maze (list (-- i) (-- j))) (write-bytes $path)
            (write-bytes $wall)))
      (write-line))))

(function generate-maze (p)
  (let (neighbors (neighbors p))
    (when (&& (.inside? $maze p)
              (nil? (.at $maze p))
              (<= (len (except nil? (map (f (q) (&& (.inside? $maze q) (.at $maze q))) neighbors))) 1))
      (.put $maze p true)
      (foreach generate-maze neighbors))))

(function! main (args)
  (rand.seed (time))
  (generate-maze '(0 0))
  (show))
