; paren unit test.

(import :optparse)

(<- $paren (.to-s (.resolve $paren-home "paren"))
    $status-cd 0
    $recur? nil
    $ignore-files nil)

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

(function punit (dir)
  (dolist (file (.children dir))
    (if (.dir? file) (if $recur? (punit file))
        (let (file-name (.to-s file))
          (print file-name)
          (if (! (&& (.file? file) (= (.suffix file) "p"))) (print "\t -- not source code")
              (in? (.basename file) $ignore-files) (print "\t -- ignore")
              (! (testable? file)) (print "\t -- skip")
              (<- $status-cd (max $status-cd (system (str $paren " " file-name)))))
          (println)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "i:r") args))
    (<- $recur? (.get op "r")
        $ignore-files (split (.get op "i") ","))
    (dolist (dir (|| args '(".")))
      (punit (path dir)))
    (exit $status-cd)))
