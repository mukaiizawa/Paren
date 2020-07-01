; Base64 module.

(function Base64.encode-6bit (b6)
  (let (url-safe? (dynamic url-safe?))
    (if (< b6 26) (+ 0x41 b6)
        (< b6 52) (+ 0x61 b6 -26)
        (< b6 62) (+ 0x30 b6 -52)
        (= b6 62) (if url-safe? 0x2d 0x2b)
        (= b6 63) (if url-safe? 0x5f 0x2f)
        (= b6 64) 0x3d)))

(function Base64.encode (src :opt url-safe?)
  ; Base64 encoding as specified by RFC 4648.
  (let (val 0 src-len (byte-array-length src) remain (mod src-len 3))
    (with-memory-stream (out)
      (with-memory-stream (in src)
        (dotimes (i (// src-len 3))
          (<- val (| (<< (read-byte in) 16) (<< (read-byte in) 8) (read-byte in)))
          (write-byte (Base64.encode-6bit (>> val 18)) out)
          (write-byte (Base64.encode-6bit (& (>> val 12) 0x3f)) out)
          (write-byte (Base64.encode-6bit (& (>> val 6) 0x3f)) out)
          (write-byte (Base64.encode-6bit (& val 0x3f)) out))
        (if (= remain 2)
            (begin
              (<- val (| (<< (read-byte in) 16) (<< (read-byte in) 8)))
              (write-byte (Base64.encode-6bit (& (>> val 18) 0x3f)) out)
              (write-byte (Base64.encode-6bit (& (>> val 12) 0x3f)) out)
              (write-byte (Base64.encode-6bit (& (>> val 6) 0x3f)) out)
              (write-byte (Base64.encode-6bit 64) out))
            (= remain 1)
            (begin
              (<- val (<< (read-byte in) 4))
              (write-byte (Base64.encode-6bit (& (>> val 6) 0x3f)) out)
              (write-byte (Base64.encode-6bit (& val 0x3f)) out)
              (write-byte (Base64.encode-6bit 64) out)
              (write-byte (Base64.encode-6bit 64) out)))))))

(function Base64.url-safe-encode (src)
  ; Base64.url-safe-encode is the alternate base64 encoding defined in RFC 4648.
  ; It is typically used in URLs and file names.
  (Base64.encode src true))

; (function Base64.decode (src :opt url-safe?)
; )

(function! main ()
  (assert (string-eq? (Base64.encode "A") "QQ=="))
  (assert (string-eq? (Base64.encode "AB") "QUI="))
  (assert (string-eq? (Base64.encode "ABC") "QUJD"))
  (assert (string-eq? (Base64.encode "ABCD") "QUJDRA=="))
  (assert (string-eq? (Base64.encode "ABCDE") "QUJDREU="))
  (assert (string-eq? (Base64.encode "ABCDEF") "QUJDREVG"))
  (assert (string-eq? (Base64.encode "ABCDEFG") "QUJDREVGRw=="))
  (assert (string-eq? (Base64.encode "abc123!?$*&()'-=@~") "YWJjMTIzIT8kKiYoKSctPUB+"))
  (assert (string-eq? (Base64.url-safe-encode "abc123!?$*&()'-=@~") "YWJjMTIzIT8kKiYoKSctPUB-")))
