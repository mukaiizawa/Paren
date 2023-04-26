; unzip.

(import :zip)

(function! main (args)
  (let (zipfile (car args) dst (|| (cadr args) (.butsuffix (path zipfile))))
    (if (nil? args) (raise ArgumentError "missing zipfile")
        (zip.uncompress zipfile dst))))
