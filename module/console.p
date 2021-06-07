; console module.

(function console.clear ()
  ; Clear screen.
  ; Returns nil.
  (.write-bytes $stdout "\x1b[2J\x1b[H")
  nil)

(function console.cursor (:key x y up down right left)
  ; Move the cursor.
  ; If x and y is specified, move to `(x, y)` coordinate within the viewport, where x is the column of the y line.
  ; If up, down, right or left is specified, cursor move to specified direction by the specified value.
  ; Returns nil.
  (if (&& (int? x) (int? y)) (.write-bytes $stdout (format "\x1b[%d;%dH" x y))
      (int? up) (.write-bytes $stdout (format "\x1b[%dA" up))
      (int? down) (.write-bytes $stdout (format "\x1b[%dB" down))
      (int? right) (.write-bytes $stdout (format "\x1b[%dC" right))
      (int? left) (.write-bytes $stdout (format "\x1b[%dD" left))
      (raise ArgumentError))
  nil)
