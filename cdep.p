; output dependency for makefile

(function parse-line (line dependencies)
  (let (open nil close nil file-name nil)
    (if (&& (string-prefix? line "#include")
            (<- open (string-index line "\""))
            (<- close (string-last-index line "\""))
            (<- file-name (string-slice line (++ open) close))
            (! (find-if (lambda (dependency)
                          (string= file-name dependency))
                        dependencies)))
        (parse-cfile (Path.of file-name) (cons file-name dependencies))
        dependencies)))

(function parse-cfile (file :opt dependencies)
  (dolist (line (.to-l file))
    (<- dependencies (parse-line line dependencies)))
  dependencies)

(function! main (args)
  ; Get C source file names from current directory and output dependency rule for makefile.
  (write-line "# following rules are generated automatically.")
  (dolist (cfile (remove-if (lambda (file)
                              (string/= (.suffix file) "c"))
                            (.children (Path.getcwd))))
    (write-line
      (list->string 
        `(,(string (.but-suffix cfile) ".o:") ,(.name cfile) ,@(parse-cfile cfile))
        " "))))
