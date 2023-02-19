; jazz hands.

(import :mouse)
(import :rand)

(function dx (x)
  (+ x (rand.choice '(-1 0 1))))

(function! main (args)
  (loop
    (catch (mouse.move (map dx (mouse.position))) identity)
    (sleep 0.05)))
