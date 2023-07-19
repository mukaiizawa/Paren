; manual unit-test generator.

(import :man (.resolve $paren-home "scripts"))

(<- $root (.mkdir (path "./wk"))
    ; Processes that are not idempotent or have side effects should be excluded from unit testing.
    $ignore-pages '("exit-quit.md"
                    "fgetc-fgets.md"
                    "fopen-fclose.md"
                    "foreach.md"
                    "getenv-putenv.md"
                    "load.md"
                    "macroexpand.md"
                    "popen-pclose.md"
                    "raise.md"
                    "read-write.md"
                    "sp-f.md"
                    "symbol.md"
                    "system.md"
                    "timeit.md"))

(function xmain (exprs)
  ;; Comparison with read-evaluated expressions does not work as a unit test because print expressions of array or byts type contain reader-macro.
  `(function! main (args)
     ,@(map (f (x)
              `(assert (= (with-memory-stream ($out) (write ,(car x) :end ""))    ; eval-print
                          (slice ,(cadr x) 4))))    ; evaluated expression
            exprs)))

(function parse-example ()
  (let (exprs nil ch nil)
    (while (<- ch (read-char))
      (if (space? ch) (continue)
          (= ch "#") (break)
          (= ch ")") (push! (list (read) (read-line)) exprs)
          (raise SyntaxError "unexpected character %v" ch)))
    (reverse! exprs)))

(function parse-man (file-name)
  (with-open ($in (.resolve $man-root file-name) :read)
    (let (line nil)
      (while (<- line (read-line))
        (when (= line "# EXAMPLES")
          (with-open ($out (.suffix (.resolve $root file-name) "p") :write)
            (println "; " file-name)
            (write (xmain (parse-example)))
            (return :succeed))))
      (return :skip))))

(function! main (args)
  (man-walk (f (section pages one-line-desc file-name)
              (when (&& (= section "3") (none? (f (x) (suffix? file-name x)) $ignore-pages))
                (write (list (parse-man file-name) file-name))
                nil))    ; traverse all manual.
            (man-indexes)))
