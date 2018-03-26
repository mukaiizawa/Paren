; paren core library.

(function print ())

(function eval ())

(function read ())

(function repl ()
  (print (eval (read))))

(repl)
