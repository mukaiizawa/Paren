; head.

(import :optparse)

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args) line nil)
    (dotimes (i (.get-int op "n" 10))
      (if (<- line (read-line)) (write-line line)
          (break)))))
