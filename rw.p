; reserved word.

(<- $reserved-words
    '(special-operator
       macro
       function
       function!
       builtin-function))

(function! main (args)
  (with-open ($in (.resolve $paren-home core.p) :read)
    (foreach write-line
             (map cadr
                  (select (f (x) (&& (cons? x) (include? (car x) $reserved-words)))
                          (collect read))))))
