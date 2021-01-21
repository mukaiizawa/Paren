; echo the string to standard output.

(import :optparse)

(function echo (newline? args)
  ; echo [OPTION] [STRING]...
  ; Echo the STRING(s) to standard output.
  ;     -n do not output the trailing newline
  (if args (write-mem (join args " ")))
  (if newline? (write-line)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n") args))
    (if args (echo (! (.get op "n")) args))))
