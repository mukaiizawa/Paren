; unzip.

(import :zip)

(function! main (args)
  (if (nil? args) (raise ArgumentError "missing zipfile")
      (let (zipfile (path (car args)))
        (zip.uncompress zipfile (|| (cadr args) (.base-name zipfile))))))
