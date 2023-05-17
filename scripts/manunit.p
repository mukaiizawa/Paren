; manual unit-test generator.

(import :man (.resolve $paren-home "scripts"))

(<- $root (.mkdir (path "./wk"))
    $ignore-pages (map string
                       '(f
                          exit quit
                          fgetc fgets
                          fopen fclose
                          foreach
                          getenv putenv
                          load
                          macroexpand
                          popen pclose
                          read read-byte read-bytes read-char read-line
                          symbol
                          system
                          timeit
                          write write-byte write-bytes write-line)))

(function xmain (exprs)
  ;; Comparison with read-evaluated expressions does not work as a unit test because print expressions of array or byts type contain reader-macro.
  `(function! main (args)
     ,@(map (f (x)
              `(assert (= (with-memory-stream ($out) (write ,(car x)))    ; eval-print
                          (concat (slice ,(cadr x) 4) "\n"))))    ; evaluated expression
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
              (when (&& (= section "3") (nil? (intersection pages $ignore-pages)))
                (write (list (parse-man file-name) file-name))
                nil))    ; traverse all manual.
            (man-indexes)))
