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
        (parse-file (Path.of file-name) (cons file-name dependencies))
        dependencies)))

(function parse-file (file :opt dependencies)
  (dolist (line (.to-l file))
    (<- dependencies (parse-line line dependencies)))
  dependencies)

(function! main (args)
  (write-bytes "# following rules are generated automatically.\n")
  (dolist (file (remove-if (lambda (file)
                             (string/= (.suffix file) "c"))
                           (.children (Path.getcwd))))
    (write-bytes
      (list->string 
        `(,(string (.but-suffix file) ".o:") ,(.name file) ,@(parse-file file))
        " "))
    (write-line)))
