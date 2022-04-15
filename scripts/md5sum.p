; md5sum.

(import :md5)
(import :optparse)

(function check (checksum path)
  (let (sum nil)
    (with-open ($in path :read) (<- sum (md5sum)))
    (format "%-20s: %s" path (if (= sum checksum) "OK" "FAILED"))))

(function md5sum ()
  (hex (md5.sum (read-bytes))))

(function comment? (line)
  (prefix? line "#"))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "c:") args)
                  check-file (.get op "c"))
    (if (nil? check-file) (write-line (md5sum))
        (foreach (f (x) (write-line (apply check (split x " "))))
                 (reject comment? (.to-l (path check-file)))))))
