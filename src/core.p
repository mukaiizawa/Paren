; paren core library.

; number
0xa
2x1010
10

; floating number
1.1

; keyword
:asdf

; nil
()

; dot list
(quote (1 . 2))
(list 1 . (list 2 3 4))

; pure list
(apply + (quote (list 1 2 3)))

(function print ())

(function eval ())

(function read ())

(function repl () (print (eval (read))))

(repl)
