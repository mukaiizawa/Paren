; output dependency for makefile

(function parse-line (line dependencies)
  (let (open-quote nil close-quote nil file-name nil)
    (if (&& (string-prefix? line "#include")
            (<- open-quote (string-index line "\""))
            (<- close-quote (string-last-index line "\""))
            (<- file-name (string-slice line (++ open-quote) close-quote))
            (! (find-if (f (x) (string= file-name x)) dependencies)))
        (parse-cfile (Path.of file-name) (cons file-name dependencies))
        dependencies)))

(function parse-cfile (file :opt dependencies)
  (dolist (line (.to-l file))
    (<- dependencies (parse-line line dependencies)))
  dependencies)

(function! main (args)
  ; Get C source file names from current directory and output dependency rule for makefile.
  (write-line "# following rules are generated automatically.")
  (dolist (cfile (remove-if (f (x) (string/= (.suffix x) "c")) (.children (Path.getcwd))))
    (write-line
      (join
        (flatten
          (list (string (.but-suffix cfile) ".o:") (.name cfile) (parse-cfile cfile)))
        " "))))
