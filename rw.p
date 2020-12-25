; reserved word.

(<- $reserved-words
    '(special-operator
       macro
       function
       function!
       builtin-function))

(function reserved-word? (x)
  (some? (f (y) (eq? x y)) $reserved-words))

(function! main (args)
  (with-open ($in (.resolve $paren-home core.p) :read)
    (foreach (f (x) (write-line (cadr x)))
             (select (f (x) (&& (cons? x) (reserved-word? (car x))))
                     (collect read)))))
