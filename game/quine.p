; Quine self-reproducing program.

(function main (args)
  (write (cons 'function (cons 'main (cons (procparams main) (procbody main))))))
