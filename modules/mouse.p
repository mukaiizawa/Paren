; mouse module.

(if (! (in? $hostname '(:windows))) (raise StateError "Unsopport OS")
    (! (bound? 'mouse.position)) (raise StateError "Requires mouse option at compile time"))

(built-in-function mouse.position ())
(built-in-function mouse.move (p))

(built-in-function mouse.fire (button dir)
  ; Do not call directly as it will change without notice in the future.
  )

; see mouse.c
(<- $mouse.direction '(:down 0 :up 1)
    $mouse.button '(:left 0 :middle 1 :right 2))

(function mouse.down (:opt button)
  (if (nil? button) (mouse.down :left)
      (mouse.fire (assoc $mouse.button button) (assoc $mouse.direction :down))))

(function mouse.up (:opt button)
  (if (nil? button) (mouse.up :left)
      (mouse.fire (assoc $mouse.button button) (assoc $mouse.direction :up))))

(function mouse.click (:opt button)
  (mouse.down button)
  (mouse.up button))

(function mouse.double-click ()
  (mouse.click)
  (sleep 0.1)
  (mouse.click))
