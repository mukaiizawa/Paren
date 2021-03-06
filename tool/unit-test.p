; unit test.

(import :optparse)

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
    (return (some? testable-main? (collect read)))))

(function paren-file? (x)
  (&& (.file? x) (= (.suffix x) "p")))

(function unit-test (dir :key recur?)
  (dolist (file (.children dir))
    (if (.dir? file) (if recur? (unit-test file :recur? true))
        (paren-file? file)
        (catch (Exception (f (e) (write-line (.to-s e))))
          (let (file-name (.to-s file))
            (write-bytes file-name) (write-bytes "\t")
            (if (testable? file) (system (str "paren " file-name))
                (write-bytes " -- skip "))
            (write-line))))))

(function! main (args)
  ; unit-test [OPTION]... [PATH]
  ; Run unit tests for the specified PATH (the current directory by default).
  ;     -r test subdirectories recursively
  (let ((op args) (.parse (.init (.new OptionParser) "r") args))
    (if (! $debug?) (raise StateError "need to build in debug mode")
        (unit-test (path (|| (car args) ".")) :recur? (.get op "r")))))
