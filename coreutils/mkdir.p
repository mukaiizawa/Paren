; make directories.

(function xmkdir (dirs)
  ; mkdir DIRECTORY...
  ; Create the DIRECTORY(ies), if they do not already exist.
  (foreach (f (x) (.mkdir (path x)))
           dirs))

(function! main (args)
  (xmkdir args))
