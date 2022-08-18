; paren shell.

(function! main (args)
  (loop
    (write-bytes "$ ") (system (str "paren " (read-line)))))
