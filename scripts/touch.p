; touch.

(function touch (file)
  (if (.none? file) (with-open ($out file :write))
      (.utime file (time))))

(function! main (args)
  (foreach touch (map path args)))
