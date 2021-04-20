; read from standard input and write to standard output and files.

(import :optparse)

(function tee (file :key append?)
  ; tee [OPTION] FILE
  ; Copy standard input to FILE, and also to standard output.
  ;     -a append to FILE.
  (let (bytes (read-bytes))
    (write-bytes bytes)
    (with-open ($out file (if append? :append :write))
      (write-bytes bytes))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "a") args))
    (tee (car args) :append? (.get op "a"))))
