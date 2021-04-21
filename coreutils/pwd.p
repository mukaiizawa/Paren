; print name of current/working directory.

(function pwd ()
  ; pwd
  ; Print the full filename of the current working directory.
  (write-line (.to-s (path.getcwd))))

(function! main (args)
  (pwd))
