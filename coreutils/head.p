; output the first part of standard input.

(import :optparse)

(function head (:opt n)
  ; head [OPTION]
  ; Print the first 10 lines of standard input.
  ;     -n print the first n lines.
  (dotimes (i (|| n 10))
    (write-line (read-line))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args) n (.get op "n"))
    (head (&& n (str->num n)))))
