; head.

(import :optparse)

(function head (n)
  (dotimes (i n)
    (write-line (read-line))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args) n (.get op "n"))
    (head (if n (int n) 10))))
