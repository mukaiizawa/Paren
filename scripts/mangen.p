; manual template generator.

(import :man (.resolve $paren-home "scripts"))

(<- $sections '("NAME"
                "SYNOPSIS"
                "DESCRIPTION"
                "OPTIONS"
                "EXIT STATUS"
                "RETURN VALUE"
                "ERRORS"
                "ENVIRONMENT"
                "FILES"
                "NOTES"
                "BUGS"
                "EXAMPLES"
                "SEE ALSO"))

(function sections (section)
  (select (f (s)
            (if (in? s '("OPTIONS" "EXIT STATUS")) (= section "1")
                (in? s '("RETURN VALUE")) (= section "3")
                true))
          $sections))

(function make-man (page section)
  (let (dir (.resolve $man-root (str "man" section)) man (.suffix (.resolve dir page) "md"))
    (println (.to-s man))
    (if (! (.dir? dir)) (raise StateError "unexpected section")
        (! (.none? man)) (raise StateError "manual already existed")
        (with-open ($out man :write)
          (dolist (s (sections section))
            (println "# " s)
            (if (= s "NAME") (println page " - one-line description of this manual.")))))))

(function! main (args)
  (let ((section page) (man-parse-command-line-args args))
    (if (|| (nil? args) (nil? section)) (raise ArgumentError "require section")
        (make-man page section))))
