; console module.

(<- $console.colors #{
    :black 30
    :read 31
    :green 32
    :yellow 33
    :blue 34
    :magenta 35
    :cyan 36
    :white 37
    })

(function console.clear ()
  ; Clear screen.
  ; Returns nil.
  (.write-bytes $stdout "\x1b[2J\x1b[H")
  nil)

(function console.color (color)
  (|| ([] $console.colors color) (raise ArgumentError "invalid color %v" color)))

(function console.bg-color (color)
  (+ (console.color color) 10))

(function console.write (text :key color bg-color)
  ; Output strings to the console in the specified color and background color.
  ; Returns ch.
  (.write-bytes $stdout
                (if (|| color bg-color) (format "\x1b[%dm\x1b[%dm%s\x1b[0m"
                                                (console.color (|| color :white))
                                                (console.bg-color (|| bg-color :black)) text)
                    text))
  text)

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
