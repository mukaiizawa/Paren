; delay for a specified amount of time.

(function xsleep (n)
  ; sleep NUMBER
  ; Pause for NUMBER seconds.
  (sleep (float n)))

(function! main (args)
  (apply xsleep args))
