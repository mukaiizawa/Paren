; concatenate files and output.

(function cat ()
  (write-bytes (read-bytes)))

(function! main (args)
  (if (nil? args) (cat)
      (foreach (f (x) (with-open ($in x :read) (cat)))
               args)))
