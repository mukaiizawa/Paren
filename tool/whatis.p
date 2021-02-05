; display summary of paren files.

(function! peek (path)
  (with-open ($in path :read)
    (write-line (string (.base-name path) " --" (submem (read-line) 1)))))

(function! paren-file? (path)
  (&& (.file? path) (memeq? (.suffix path) "p")))

(function! main (args)
  (foreach peek (select paren-file? (.children (Path.getcwd)))))
