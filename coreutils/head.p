; output the first part of standard input.

(import :optparse)

(function head (n)
  ; head [OPTION]
  ; Print the first 10 lines of standard input.
  ;     -n print the first n lines.
  (dotimes (i n)
    (write-line (read-line))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args) n (.get op "n"))
    (head (if n (int n) 10))))
