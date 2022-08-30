; reserved word.

(<- $reserved-words
    '(special-operator
       macro
       function
       function!
       built-in-function))

(function! main (args)
  (with-open ($in (.resolve $paren-home "modules/core.p") :read)
    (foreach write-line
             (map cadr
                  (select (f (x) (&& (cons? x) (in? (car x) $reserved-words)))
                          (collect read))))))
