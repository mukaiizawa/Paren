; paren core library.

0x12

1.1
:asdf
(function print ())

(function eval ())

(function read ())

(function repl ()
  (print (eval (read))))

(repl)
