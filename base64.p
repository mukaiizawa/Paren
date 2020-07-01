; Base64 module.

(global-symbol Base64.mapping "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(global-symbol Base64.url-mapping "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
(global-symbol Base64.padding "=")

(function Base64.encode (src :key (mapping Base64.mapping) (padding Base64.padding))
  ; Base64 encoding as specified by RFC 4648.
  (let (val 0 src-len (byte-array-length src) remain (mod src-len 3))
    (with-memory-stream (out)
      (with-memory-stream (in src)
        (dotimes (i (// src-len 3))
          (<- val (bit-or (bit-shift (read-byte in) 16)
                          (bit-shift (read-byte in) 8)
                          (read-byte in)))
          (write-byte (byte-array-at mapping (bit-and (bit-shift val -18) 0x3f)) out)
          (write-byte (byte-array-at mapping (bit-and (bit-shift val -12) 0x3f)) out)
          (write-byte (byte-array-at mapping (bit-and (bit-shift val -6) 0x3f)) out)
          (write-byte (byte-array-at mapping (bit-and val 0x3f)) out))
        (when (/= remain 0)
          (<- val (bit-shift (read-byte in) 16))
          (if (= remain 2) (<- val (bit-or val (bit-shift (read-byte in) 8))))
          (write-byte (byte-array-at mapping (bit-and (bit-shift val -18) 0x3f)) out)
          (write-byte (byte-array-at mapping (bit-and (bit-shift val -12) 0x3f)) out)
          (if (= remain 2)
              (begin
                (write-byte (byte-array-at mapping (bit-and (bit-shift val -6) 0x3f)) out)
                (write-string padding out))
              (begin
                (write-string padding out)
                (write-string padding out))))))))

(function Base64.url-encode (src)
  ; Base64.url-encode is the alternate base64 encoding defined in RFC 4648.
  ; It is typically used in URLs and file names.
  (Base64.encode src :mapping Base64.url-mapping))

; (function Base64.decode (src)
;   (let (si 0 di 0 src-len (byte-array-length src) n (* (// src-len 3) 3) val 0)
;     (with-memory-stream (out)
;       (with-memory-stream (in src)

(function! main ()
  (assert (string-eq? (Base64.encode "A") "QQ=="))
  (assert (string-eq? (Base64.encode "AB") "QUI="))
  (assert (string-eq? (Base64.encode "ABC") "QUJD"))
  (assert (string-eq? (Base64.encode "ABCD") "QUJDRA=="))
  (assert (string-eq? (Base64.encode "ABCDE") "QUJDREU="))
  (assert (string-eq? (Base64.encode "ABCDEF") "QUJDREVG"))
  (assert (string-eq? (Base64.encode "ABCDEFG") "QUJDREVGRw=="))
  (assert (string-eq? (Base64.encode "abc123!?$*&()'-=@~") "YWJjMTIzIT8kKiYoKSctPUB+"))
  (assert (string-eq? (Base64.url-encode "abc123!?$*&()'-=@~") "YWJjMTIzIT8kKiYoKSctPUB-")))
