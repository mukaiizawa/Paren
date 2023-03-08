; unzip.

(import :zip)

(function! main (args)
  (let (zipfile (car args) dst (|| (cadr args) (.but-suffix (path zipfile))))
    (if (nil? args) (raise ArgumentError "missing zipfile")
        (zip.uncompress zipfile dst))))
