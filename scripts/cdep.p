; output dependency for makefile.

(function parse-line (line dependencies)
  (let (file nil)
    (if (&& (in? "#include" line) (<- file (cadr (split line "\""))) (! (in? file dependencies)))
        (parse-cfile (path file) (cons file dependencies))
        dependencies)))

(function parse-cfile (file :opt dependencies)
  (dolist (line (.to-l file))
    (<- dependencies (parse-line line dependencies)))
  dependencies)

(function! main (args)
  ; cdep
  ; Get C source file names from current directories and output dependency rule for makefile.
  (write-line "# following rules are generated automatically.")
  (foreach write-line
           (map (f (cfile)
                  (join (cons (str (.but-suffix cfile) ".o:")
                              (cons (.name cfile) (parse-cfile cfile)))
                        " "))
                (select (f (x) (= (.suffix x) "c"))
                        (.children (path.getcwd))))))
