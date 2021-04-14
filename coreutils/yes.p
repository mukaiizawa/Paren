; output a string repeatedly until killed.

(function yes (s)
  ; yes [STRING]
  ; Repeatedly output a line with all specified STRING, or 'y'.
  (write-line s)
  (yes s))

(function main (args)
  (catch (Exception (f (x) nil))
    (yes (|| (car args) "y"))))
