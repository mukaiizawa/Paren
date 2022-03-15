; prevent sleep.

(import :mouse)

(<- $origin '(0 0)
    $distance-threshold 100)

(function stopped? (p q)
  (< (sqrt
       (apply +
              (map (f (dx) (pow dx 2))
                   (map - p q))))
     $distance-threshold))

(function! main (args)
  (let (prev (mouse.position))
    (while (stopped? prev (<- prev (mouse.position)))
      (foreach mouse.move (list $origin prev))
      (sleep 1))))
