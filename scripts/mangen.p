; manual template generator.

(import :man (.resolve $paren-home "scripts"))

(function wirte-man (page section)
  (let (dir (.resolve $man-root (str "man" section)) man (.suffix (.resolve dir page) "md"))
    (write-line (.to-s man))
    (if (! (.dir? dir)) (raise StateError "unexpected section")
        (! (.none? man)) (raise StateError "manual already existed")
        (with-open ($out man :write)
          (foreach (f (x)
                     (if (suffix? x ".") (write-line x)
                         (= x "NAME") (write-line "# NAME")
                         (write-line (str "\n# " x))))
                   `("NAME" ,(str page " - one-line description of this manual.")
                     "SYNOPSIS"
                     "DESCRIPTION"
                     ,@(if (= section "1") '("OPTIONS" "EXIT STATUS")
                           (= section "3") '("RETURN VALUE"))
                     "ERRORS"
                     "ENVIRONMENT"
                     "FILES"
                     "NOTES"
                     "BUGS"
                     "EXAMPLES"
                     "SEE ALSO"))))))

(function! main (args)
  (let ((section page) (man-parse-command-line-args args))
    (if (|| (nil? args) (nil? section)) (raise ArgumentError "require section")
        (wirte-man page section))))
