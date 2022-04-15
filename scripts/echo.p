; echo.

(import :optparse)

(function echo (newline? args)
  (if args (write-bytes (join args " ")))
  (if newline? (write-line)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n") args))
    (if args (echo (! (.get op "n")) args))))
