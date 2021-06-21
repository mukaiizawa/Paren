; reserved word.

(<- $reserved-words
    '(special-operator
       macro
       function
       function!
       builtin-function))

(function! main (args)
  (timeit
  (with-open ($in (.resolve (.resolve $paren-home "module") core.p) :read)
    (foreach write-line
             (map cadr
                  (select (f (x) (&& (cons? x) (in? (car x) $reserved-words)))
                          (collect read)))))))
