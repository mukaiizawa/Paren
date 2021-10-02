; Quine self-reproducing program.

(function! main (args)
  (write (cons 'function! (cons 'main (cons (params main) (body main))))))
