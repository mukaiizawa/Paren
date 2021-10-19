; manual unit-test generator.

(import :man (.resolve $paren-home "tools"))

(<- $root (.mkdir (path "./wk"))
    $ignore-pages (map string
                       '(f timeit macroexpand)))

(function xmain (exprs)
  `(function! main (args)
     ,@(map (f (expr) `(assert (= ,@expr)))
            exprs)))

(function write-log (key)
  (.write-line $stdout (str (dynamic file-name) " -- " key)))

(class IllegalManError (StateError))

(function parse-example ()
  (let (exprs nil ch nil)
    (while (<- ch (read-char))
      (if (space? ch) (continue)
          (= ch "#") (break)
          (= ch ")") (push! (list (read) (list quote (read))) exprs)
          (raise StateError ch)))
    (reverse! exprs)))

(function md->p (file-name)
  (str (.base-name (path file-name)) ".p"))

(function parse-man (file-name)
  (catch (IllegalManError (f (e) (write-log :failed)))
    (with-open ($in (.resolve $man-root file-name) :read)
      (let (line nil)
        (while (<- line (read-line))
          (when (= line "# EXAMPLES")
            (with-open ($out (.resolve $root (md->p file-name)) :write)
              (write-line (str "; " file-name))
              (write (xmain (parse-example)))
              (write-log :succeed)
              (return nil))))
        (write-log :skip)))))

(function! main (args)
  (man-walk (f (section pages one-line-desc file-name)
              (if (&& (= section "3") (nil? (intersection pages $ignore-pages))) (parse-man file-name)))
            (man-indexes)))
