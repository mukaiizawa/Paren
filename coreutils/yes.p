; output a string repeatedly until killed.

(function yes (s)
  (write-line s)
  (yes s))

(function main (args)
  (yes (|| (car args) "y")))
