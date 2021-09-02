; tee.

(import :optparse)

(function tee (file :key append?)
  (let (bytes (read-bytes))
    (write-bytes bytes)
    (with-open ($out file (if append? :append :write))
      (write-bytes bytes))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "a") args))
    (tee (car args) :append? (.get op "a"))))
