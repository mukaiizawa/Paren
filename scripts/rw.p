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
             (keep (f (x) (if (&& (cons? x) (in? (car x) $reserved-words)) (cadr x)))
                   (collect read)))))
