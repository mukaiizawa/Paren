; encode and decode using Base64 representation.

(import :optparse)
(import :base64)

(function base64 (:key decode?)
  ; base64 [OPTION]...
  ; encode / decode binary file as RFC 1341 MIME base64.
  ;     -d decodes the standard input, previously created by base64, to recover the original input.
  (let (read-mem (f ()
                   (with-memory-stream ($out)
                     (let (b nil)
                       (while (!= (<- b (read-byte)) -1)
                         (write-byte b)))))
                 mem (read-mem))
    (write-mem (if decode? (base64.decode mem)
                   (base64.encode mem)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "d") args))
    (base64 :decode? (.get op "d"))))
