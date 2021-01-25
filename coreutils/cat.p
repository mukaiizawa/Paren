; concatenate files and output.

(function cat ()
  (let (c nil)
    (while (/= (<- c (read-byte)) -1)
      (write-byte c))))

(function! main (args)
  (if args (foreach (f (x) (with-open ($in x :read) (cat)))
                    args)
      (cat)))
