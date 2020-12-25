; unit test.

(function unit-test (p)
  (let (pathstr (.to-s p))
    (write-line pathstr)
    (system (string "paren " pathstr))))

(function peek-line (path)
  (with-open ($in path :read)
    (return (read-line))))

(function! module-file? (path)
  (&& (.file? path)
      (memeq? (.suffix path) "p")
      (memsuffix? (peek-line path) "module.")))

(function! main (args)
  (write :unit-test)
  (foreach unit-test (select module-file? (.children $paren-home))))
