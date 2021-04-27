; display summary of paren files.

(function! peek (path)
  (with-open ($in path :read)
    (write-line (str (.base-name path) " --" (slice (read-line) 1)))))

(function! paren-file? (path)
  (&& (.file? path) (= (.suffix path) "p")))

(function! main (args)
  (foreach peek (select paren-file? (.children (path.getcwd)))))
