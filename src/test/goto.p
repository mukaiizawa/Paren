; goto statement

(macro while (test :rest body)
  (list (list lambda nil
              (cons labels
                    (cons :while
                          (cons (list if (list not test) (list return nil))
                                (add body '(goto :while))))))))

(<- a 0)
(while !(= a 8)
       (if (= a 4) (return))
       (print (<- a (++ a))))
