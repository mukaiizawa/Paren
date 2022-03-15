; prevent sleep.

(import :mouse)

(<- $distance-threshold 50)

(function stopped? (p q)
  (< (sqrt
       (apply +
              (map (f (dx) (pow dx 2))
                   (map - p q))))
     $distance-threshold))

(function! main (args)
  (let (prev (mouse.position))
    (while (stopped? prev (mouse.position))
      (foreach mouse.move '((0 0) (30 30)))
      (<- prev (mouse.position))
      (sleep 1))))
