; compute and check MD5 message digest.

(import :md5)
(import :optparse)

(function check (checksum path)
  (let (sum nil)
    (with-open ($in path :read) (<- sum (md5sum)))
    (str path ": " (if (= sum checksum) "OK" "FAILED"))))

(function check-line (line)
  (let ((checksum path) (split line " "))
    (check checksum path)))

(function md5sum ()
  ; md5sum [OPTION]
  ; Print or check MD5 (128-bit) checksums.
  ;     -c FILE read MD5 sums from the FILEs and check them
  (let (mem (with-memory-stream ($out)
              (let (ch nil)
              (while (!= (<- ch (read-byte)) -1) (write-byte ch)))))
    (hexstr (md5.sum mem))))

(function comment? (line)
  (memprefix? line "#"))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "c:") args)
                  check-file (.get op "c"))
    (if (nil? check-file) (write-line (md5sum))
        (foreach write-line
                 (map check-line
                      (except comment? (.to-l (Path.of check-file))))))))
