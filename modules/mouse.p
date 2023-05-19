; mouse module.

(if (! (in? $hostname '(:windows))) (raise StateError "Unsupported OS")
    (! (bound? 'mouse.position)) (raise StateError "Requires mouse option at compile time"))

(built-in-function mouse.position ())
(built-in-function mouse.move (p))

(built-in-function mouse.send (button dir)
  ; Do not call directly as it will change without notice in the future.
  )

; see mouse.c
(<- $mouse.buttons '(:left :middle :right))

(function mouse.down (:opt button)
  (if (nil? button) (mouse.down :left)
      (mouse.send (index button $mouse.buttons) 0)))

(function mouse.up (:opt button)
  (if (nil? button) (mouse.up :left)
      (mouse.send (index button $mouse.buttons) 1)))

(function mouse.click (:opt button)
  (mouse.down button)
  (mouse.up button))

(function mouse.double-click ()
  (mouse.click)
  (sleep 0.1)
  (mouse.click))
