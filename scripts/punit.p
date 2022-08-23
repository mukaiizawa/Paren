; paren unit test.

(import :optparse)

(<- $paren (.to-s (.resolve $paren-home "paren"))
    $status-cd 0)

(function testable-main? (tree)
  (let (assert-expr? (f (x)
                       (&& (cons? x)
                           (|| (== (car x) 'assert)
                               (some? assert-expr? (cdr x))))))
    (&& (== (car tree) 'function!)
        (== (cadr tree) 'main)
        (some? assert-expr? (cddr tree)))))

(function testable? (x)
  (with-open ($in x :read)
    (some? testable-main? (collect read))))

(function paren-file? (x)
  (&& (.file? x) (= (.suffix x) "p")))

(function punit (dir :key recur?)
  (dolist (file (.children dir))
    (if (.dir? file) (if recur? (punit file :recur? true))
        (paren-file? file)
        (let (file-name (.to-s file))
          (write-bytes file-name) (write-bytes "\t")
          (if (! (testable? file)) (write-bytes " -- skip ")
              (<- $status-cd (max $status-cd (system (str $paren " " file-name)))))
          (write-line))))))

(function! main (args)
  ; punit [OPTION]... [PATH]
  ; Run unit tests for the specified PATH (the current directory by default).
  ;     -r test subdirectories recursively
  (let ((op args) (.parse (.init (.new OptionParser) "r") args))
    (punit (path (|| (car args) ".")) :recur? (.get op "r"))
    (exit $status-cd)))
