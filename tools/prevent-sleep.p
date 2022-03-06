; prevent sleep.

(import :mouse)

(function! main (args)
  (let (prev (mouse.position))
    (while (= prev (mouse.position))
      (mouse.move '(0 0))
      (mouse.move '(10 10))
      (<- prev (mouse.position))
      (sleep 1))))
