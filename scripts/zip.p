; zip.

(import :zip)

(function! main (args)
  (if (nil? args) (raise ArgumentError)
      (let (dir (car args))
        (zip.compress dir (|| (cadr args) (str dir ".zip"))))))
