; base64.

(import :optparse)
(import :base64)

(function base64 (:key decode?)
  ; RFC 1341 MIME base64.
  (let (bytes (read-bytes))
    (write-bytes (if (nil? decode?) (base64.encode bytes)
                     (base64.decode bytes)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "d") args))
    (base64 :decode? (.get op "d"))))
