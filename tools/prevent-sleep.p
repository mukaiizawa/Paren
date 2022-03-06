; prevent sleep.

(import :mouse)

(function! main (args)
  (let (prev (mouse.position))
    (while (= prev (mouse.position))
      (foreach mouse.move '((0 0) (10 10)))
      (<- prev (mouse.position))
      (sleep 1))))
