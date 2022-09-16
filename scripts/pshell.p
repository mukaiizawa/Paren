; paren shell.

(function! main (args)
  (loop
    (print "$ ") (system (str "paren " (read-line)))))
