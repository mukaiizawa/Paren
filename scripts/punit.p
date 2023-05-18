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
          (print file-name "\t")
          (if (! (testable? file)) (print " -- skip ")
              (<- $status-cd (max $status-cd (system (str $paren " " file-name)))))
          (println))))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "r") args))
    (dolist (dir (|| args '(".")))
      (punit (path dir) :recur? (.get op "r")))
    (exit $status-cd)))
