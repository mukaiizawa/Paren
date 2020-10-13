; output dependency for makefile.

(function parse-line (line dependencies)
  (let (open-quote nil close-quote nil file-name nil)
    (if (&& (memprefix? line "#include")
            (<- open-quote (strstr line "\""))
            (<- close-quote (strlstr line "\""))
            (<- file-name (substr line (++ open-quote) close-quote))
            (none? (f (x) (memeq? file-name x)) dependencies))
        (parse-cfile (Path.of file-name) (cons file-name dependencies))
        dependencies)))

(function parse-cfile (file :opt dependencies)
  (dolist (line (.to-l file))
    (<- dependencies (parse-line line dependencies)))
  dependencies)

(function! main (args)
  ; Get C source file names from current directory and output dependency rule for makefile.
  (write-line "# following rules are generated automatically.")
  (dolist (cfile (except (f (x) (memneq? (.suffix x) "c")) (.children (Path.getcwd))))
    (write-line
      (join
        (append (list (string (.but-suffix cfile) ".o:") (.name cfile)) (parse-cfile cfile))
        " "))))
