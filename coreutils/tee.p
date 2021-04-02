; read from standard input and write to standard output and files.

(import :optparse)

(function tee (file :key append?)
  ; tee FILE
  ; Copy standard input to FILE, and also to standard output.
  ;     -a append to FILE.
  (with-open ($out file (if append? :append :write))
    (let (c nil)
      (while (!= (<- c (read-byte)) -1)
        (let ($out $stdout) (write-byte c))
        (write-byte c)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "a") args))
    (tee (car args) :append? (.get op "a"))))
