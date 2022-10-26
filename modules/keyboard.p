; keyboard module.

(function! startup ()
  (if (! (in? $hostname '(:windows))) (raise StateError "Unsopport OS")
      (! (bound? 'keyboard.send)) (raise StateError "Requires keyboard option at compile time")))

(built-in-function keyboard.send (keycode dir)
  ; Do not call directly as it will change without notice in the future.
  )

(function keyboard.keycode (key)
  ;; todo Apply to symbols, etc.
  (if (&& (string? key) (alnum? key)) (ord (upper key))
      (== key :esc) 0x1b
      (== key :shift) 0x10
      (== key :ctrl) 0x11
      (== key :alt) 0x12
      (== key :tab) 0x09
      (== key :space) 0x20
      (== key :backspace) 0x08
      (== key :down) 0x28
      (== key :up) 0x26
      (== key :right) 0x27
      (== key :left) 0x25
      (== key :ins) 0x2d
      (== key :del) 0x2e
      (== key :home) 0x24
      (== key :end) 0x23
      (== key :pageup) 0x21
      (== key :pagedown) 0x22
      (== key :scr) 0x2c
      (== key :f1) 0x70
      (== key :f2) 0x71
      (== key :f3) 0x72
      (== key :f4) 0x73
      (== key :f5) 0x74
      (== key :f6) 0x75
      (== key :f7) 0x76
      (== key :f8) 0x77
      (== key :f9) 0x78
      (== key :f10) 0x79
      (== key :f11) 0x7a
      (== key :f12) 0x7b
      (raise ArgumentError "illegal key")))

(function keyboard.down (key)
  (keyboard.send (keyboard.keycode key) 0))

(function keyboard.up (key)
  (keyboard.send (keyboard.keycode key) 1))

(function keyboard.press (key)
  (keyboard.down key)
  (keyboard.up key))

(macro with-keydown (key :rest body)
  `(unwind-protect
     (begin (keyboard.down ,key) ,@body)
     (keyboard.up ,key)))
