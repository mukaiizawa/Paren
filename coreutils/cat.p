; concatenate files and output.

(function cat ()
  ; cat [FILE]...
  ; Concatenate FILE(s) to standard output.
  (write-bytes (read-bytes)))

(function! main (args)
  (if (nil? args) (cat)
      (foreach (f (x) (with-open ($in x :read) (cat)))
               args)))
