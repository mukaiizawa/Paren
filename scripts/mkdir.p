; mkdir.

(function! main (args)
  (foreach (f (x) (.mkdir (path x)))
           args))
