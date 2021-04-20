; encode and decode using Base64 representation.

(import :optparse)
(import :base64)

(function base64 (:key decode?)
  ; base64 [OPTION]...
  ; encode / decode binary file as RFC 1341 MIME base64.
  ;     -d decodes the standard input, previously created by base64, to recover the original input.
  (let (bytes (read-bytes))
    (write-bytes (if (nil? decode?) (base64.encode bytes)
                     (base64.decode bytes)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "d") args))
    (base64 :decode? (.get op "d"))))
