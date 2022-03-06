; console module.

(function console.clear ()
  ; Clear screen.
  ; Returns nil.
  (.write-bytes $stdout "\x1b[2J\x1b[H")
  nil)

(function console.color (color)
  (if (== color :black) 30
      (== color :read) 31
      (== color :green) 32
      (== color :yellow) 33
      (== color :blue) 34
      (== color :magenta) 35
      (== color :cyan) 36
      (== color :white) 37
      (console.color :black)))

(function console.bg-color (color)
  (if (== color :black) 40
      (== color :read) 41
      (== color :green) 42
      (== color :yellow) 43
      (== color :blue) 44
      (== color :magenta) 45
      (== color :cyan) 46
      (== color :white) 47
      (console.color :black)))

(function console.write (ch :key color bg-color)
  ; Output strings to the console in the specified color and background color.
  ; Returns ch.
  (if (! (|| color bg-color)) (.write-bytes $stdout ch)
      (.write-bytes $stdout (format "\x1b[%dm\x1b[%dm%s\x1b[0m" (console.color color) (console.bg-color bg-color) ch)))
  ch)

(function console.cursor (:key x y up down right left)
  ; Move the cursor.
  ; If x and y is specified, move to `(x, y)` coordinate within the viewport, where x is the column of the y line.
  ; If up, down, right or left is specified, cursor move to specified direction by the specified value.
  ; Returns nil.
  (if (&& (int? x) (int? y)) (.write-bytes $stdout (format "\x1b[%d;%dH" (++ y) (++ x)))
      (int? up) (.write-bytes $stdout (format "\x1b[%dA" up))
      (int? down) (.write-bytes $stdout (format "\x1b[%dB" down))
      (int? right) (.write-bytes $stdout (format "\x1b[%dC" right))
      (int? left) (.write-bytes $stdout (format "\x1b[%dD" left))
      (raise ArgumentError))
  nil)
