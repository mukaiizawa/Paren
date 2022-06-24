; paren shell.

(function! main (args)
  (loop
    (catch (Error .print-stack-trace)
      (write-bytes "$ ") (system (str "paren " (read-line))))))
