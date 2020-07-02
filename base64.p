; Base64 module.

(function Base64.encode-6bit (b6)
  (let (url-safe? (dynamic url-safe?))
    (if (< b6 26) (+ 0x41 b6)
        (< b6 52) (+ 0x61 b6 -26)
        (< b6 62) (+ 0x30 b6 -52)
        (= b6 62) (if url-safe? 0x2d 0x2b)
        (= b6 63) (if url-safe? 0x5f 0x2f)
        (= b6 64) 0x3d
        (assert nil))))

(function Base64.encode (src :opt url-safe?)
  ; Base64 encoding as specified by RFC 4648.
  (let (val 0 src-len (byte-array-length src) remain (mod src-len 3))
    (with-memory-stream (out)
      (with-memory-stream (in src)
        (dotimes (i (// src-len 3))
          (<- val (| (<< (read-byte in) 16)
                     (<< (read-byte in) 8)
                     (read-byte in)))
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

(function Base64.decode-byte (b :opt possible-padding?)
  (if (<= 0x41 b 0x5a) (- b 0x41)
      (<= 0x61 b 0x7a) (+ (- b 0x61) 26)
      (<= 0x30 b 0x39) (+ (- b 0x30) 52)
      (|| (= b 0x2d) (= b 0x2b)) 62
      (|| (= b 0x5f) (= b 0x2f)) 63
      (&& possible-padding? (|| (= b -1) (= b 0x3d))) nil
      (error "illegal base64")))

(function Base64.decode (src)
  ; Base64 decoding as specified by RFC 4648.
  ; Processing after the padding character `=` is not performed.
  (let (val 0 b6 0 size 0)
    (with-memory-stream (out)
      (with-memory-stream (in src)
        (while (&& (/= (<- val (read-byte in)) -1) b6)
          (<- val (| (<< (Base64.decode-byte val) 18)
                     (<< (Base64.decode-byte (read-byte in)) 12)))
          (if (! (<- b6 (Base64.decode-byte (read-byte in) true))) (<- size 1)
              (if (! (<- val (| val (<< b6 6))
                         b6 (Base64.decode-byte (read-byte in) true))) (<- size 2)
                  (<- val (| val b6)
                      size 3)))
          (write-byte (>> val 16) out)
          (if (>= size 2) (write-byte (& (>> val 8) 0xff) out))
          (if (= size 3) (write-byte (& val 0xff) out)))))))

(function! main ()
  (assert (string-eq? (Base64.encode "") ""))
  (assert (string-eq? (Base64.encode "A") "QQ=="))
  (assert (string-eq? (Base64.encode "AB") "QUI="))
  (assert (string-eq? (Base64.encode "ABC") "QUJD"))
  (assert (string-eq? (Base64.encode "ABCD") "QUJDRA=="))
  (assert (string-eq? (Base64.encode "ABCDE") "QUJDREU="))
  (assert (string-eq? (Base64.encode "ABCDEF") "QUJDREVG"))
  (assert (string-eq? (Base64.encode "ABCDEFG") "QUJDREVGRw=="))
  (assert (string-eq? (Base64.encode "abc123!?$*&()'-=@~") "YWJjMTIzIT8kKiYoKSctPUB+"))
  (assert (string-eq? (Base64.url-safe-encode "abc123!?$*&()'-=@~") "YWJjMTIzIT8kKiYoKSctPUB-"))
  (assert (string-eq? (Base64.decode (Base64.encode "")) ""))
  (assert (string-eq? (Base64.decode (Base64.encode "A")) "A"))
  (assert (string-eq? (Base64.decode (Base64.encode "AB")) "AB"))
  (assert (string-eq? (Base64.decode (Base64.encode "ABC")) "ABC"))
  (assert (string-eq? (Base64.decode (Base64.encode "ABCD")) "ABCD"))
  (assert (string-eq? (Base64.decode (Base64.encode "ABCDE")) "ABCDE"))
  (assert (string-eq? (Base64.decode (Base64.encode "ABCDEF")) "ABCDEF"))
  (assert (string-eq? (Base64.decode (Base64.encode "ABCDEFG")) "ABCDEFG"))
  (assert (string-eq? (Base64.decode (Base64.encode "abc123!?$*&()'-=@~")) "abc123!?$*&()'-=@~"))
  (assert (string-eq? (Base64.decode (Base64.url-safe-encode "abc123!?$*&()'-=@~")) "abc123!?$*&()'-=@~")))
