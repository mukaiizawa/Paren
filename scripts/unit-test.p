; unit test.

(import :optparse)

(<- $paren-path (.to-s (.resolve $paren-home "paren"))
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

(function unit-test (dir :key recur?)
  (dolist (file (.children dir))
    (if (.dir? file) (if recur? (unit-test file :recur? true))
        (paren-file? file)
        (catch (Exception (f (e) (write-line (.to-s e))))
          (let (file-name (.to-s file))
            (write-bytes file-name) (write-bytes "\t")
            (if (! (testable? file)) (write-bytes " -- skip ")
                (let (sc (system (str $paren-path " " file-name)))
                  (if (!= sc 0) (<- $status-cd sc))))
            (write-line))))))

(function! main (args)
  ; unit-test [OPTION]... [PATH]
  ; Run unit tests for the specified PATH (the current directory by default).
  ;     -r test subdirectories recursively
  (let ((op args) (.parse (.init (.new OptionParser) "r") args))
    (if (! $debug?) (raise StateError "need to build in debug mode")
        (unit-test (path (|| (car args) ".")) :recur? (.get op "r")))
    (exit $status-cd)))
