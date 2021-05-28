; unit test.

(function unit-test (p)
  (let (pathstr (.to-s p))
    (write-line pathstr)
    (system (str "paren " pathstr))))

(function peek-line (path)
  (with-open ($in path :read)
    (return (read-line))))

(function! module-file? (path)
  (&& (.file? path)
      (= (.suffix path) "p")
      (suffix? (peek-line path) "module.")))

(function! main (args)
  (let (debug? nil)
    (assert (<- debug? true))
    (if (! debug?) (raise StateError "need to build in debug mode")
        (foreach unit-test (select module-file? (.children (path.getcwd)))))))
