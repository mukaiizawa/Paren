; delay for a specified amount of time.

(function xsleep (n)
  ; sleep NUMBER
  ; Pause for NUMBER seconds.
  (sleep (str->num n)))

(function! main (args)
  (apply xsleep args))
