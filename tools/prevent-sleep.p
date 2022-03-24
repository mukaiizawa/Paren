; prevent sleep.

(import :mouse)
(import :rand)

(<- $HD '(720 1280)
    $interval 120)

(function! main (args)
  (loop
    (mouse.move (map rand.int $HD))
    (sleep $interval)))
