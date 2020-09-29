; reserved word.

(<- $reserved-words
    '(special-operator
       macro
       function
       function!
       builtin-function))

(function! main (args)
  (with-open (in (.resolve $paren-home core.p) :read)
    (dolist (expr (read-all in))
      (if (&& (cons? expr)
              (find-if (lambda (x)
                         (eq? x (car expr)))
                       $reserved-words))
          (write (cadr expr))))))
