; output dependency for makefile.

(<- $usage
"
Usage: paren cdep.p
	Get C source file names from current directories and output dependency rule for makefile.
")

(function parse-line (line dependencies)
  (let (open-quote nil close-quote nil file-name nil)
    (if (&& (memprefix? line "#include")
            (<- open-quote (strstr line "\""))
            (<- close-quote (strlstr line "\""))
            (<- file-name (substr line (++ open-quote) close-quote))
            (! (include? file-name dependencies)))
        (parse-cfile (path file-name) (cons file-name dependencies))
        dependencies)))

(function parse-cfile (file :opt dependencies)
  (dolist (line (.to-l file))
    (<- dependencies (parse-line line dependencies)))
  dependencies)

(function! main (args)
  (catch (Error (f (e) (write-line $usage) (throw e)))
    (write-line "# following rules are generated automatically.")
    (foreach (f (cfile)
               (write-line (join (cons (str (.but-suffix cfile) ".o:")
                                       (cons (.name cfile) (parse-cfile cfile)))
                                 " ")))
             (select (f (x) (= (.suffix x) "c"))
                     (.children (path.getcwd))))))
