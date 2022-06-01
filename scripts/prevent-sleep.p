; prevent sleep.

(import :mouse)
(import :rand)

(function dx (x)
  (+ x (rand.choice '(-1 0 1))))

(function! main (args)
  (loop
    (catch (Error (f (e) nil))
      (mouse.move (map dx (mouse.position))))
    (sleep 0.1)))
