; reserved word

(global-symbol $reserved-words
  '(global-symbol
     special-operator
     macro
     function
     function!
     builtin-function))

(function! main ()
  (dolist (reserved-word $reserved-words)
    (write reserved-word))
  (with-open-read (in (.resolve $paren-home core.p))
    (let (expr nil)
      (while (neq? (<- expr (read in)) :EOF)
        (if (&& (cons? expr))
            (dolist (reserved-word $reserved-words)
              (when (eq? (car expr) reserved-word)
                (write (cadr expr))
                (break))))))))
