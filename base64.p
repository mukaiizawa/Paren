; base64 module.

(function base64.encode (src :opt url-safe?)
  ; base64 encoding as specified by RFC 4648.
  ; Returns a base64 encoded string.
  (let (val 0 b1 0 b2 0 b3 0 size 3
            encode (f (bits)
                     (if (nil? bits) 0x3d    ; padding
                         (< bits 26) (+ 0x41 bits)
                         (< bits 52) (+ 0x61 bits -26)
                         (< bits 62) (+ 0x30 bits -52)
                         (= bits 62) (if url-safe? 0x2d 0x2b)
                         (= bits 63) (if url-safe? 0x5f 0x2f)
                         (assert nil))))
    (with-memory-stream ($out)
      (with-memory-stream ($in src)
        (while (= size 3)
          (if (= (<- b1 (read-byte)) -1) (break)
              (= (<- b2 (read-byte)) -1) (<- size 1 b2 0)
              (= (<- b3 (read-byte)) -1) (<- size 2 b3 0)
              (<- size 3))
          (<- val (| (<< b1 16) (<< b2 8) b3))
          (write-byte (encode (>> val 18)))
          (write-byte (encode (& (>> val 12) 0x3f)))
          (write-byte (encode (&& (> size 1) (& (>> val 6) 0x3f))))
          (write-byte (encode (&& (> size 2) (& val 0x3f)))))))))

(function base64.url-safe-encode (src)
  ; base64.url-safe-encode is the alternate base64 encoding defined in RFC 4648.
  ; It is typically used in URLs and file names.
  ; Returns a base64 encoded string.
  (base64.encode src true))

(function base64.decode-byte (b :opt accept-padding?)
  (if (<= 0x41 b 0x5a) (- b 0x41)
      (<= 0x61 b 0x7a) (+ (- b 0x61) 26)
      (<= 0x30 b 0x39) (+ (- b 0x30) 52)
      (|| (= b 0x2d) (= b 0x2b)) 62
      (|| (= b 0x5f) (= b 0x2f)) 63
      (&& (= b 0x3d) accept-padding?) nil
      (error "illegal base64")))

(function base64.decode (src)
  ; base64 decoding as specified by RFC 4648.
  ; Processing after the padding character `=` is not performed.
  ; Returns a decoded string.
  (let (val 0 x 0 b1 0 b2 0 b3 0 b4 0 size 3)
    (with-memory-stream ($out)
      (with-memory-stream ($in src)
        (while (= size 3)
          (if (= (<- b1 (read-byte)) -1) (break)
              (= (<- b2 (read-byte)) -1) (<- b2 0x3d b3 b2 b4 b2)
              (= (<- b3 (read-byte)) -1) (<- b3 0x3d b4 b3)
              (= (<- b4 (read-byte)) -1) (<- b4 0x3d))
          (<- val (| (<< (base64.decode-byte b1) 18)
                     (<< (base64.decode-byte b2) 12)))
          (if (! (<- x (base64.decode-byte b3 true))) (<- size 1)
              (! (<- val (| val (<< x 6))
                     x (base64.decode-byte b4 true))) (<- size 2)
              (<- val (| val x) size 3))
          (write-byte (>> val 16))
          (if (>= size 2) (write-byte (& (>> val 8) 0xff)))
          (if (= size 3) (write-byte (& val 0xff))))))))

(function! main (args)
  (assert (memeq? (base64.encode "") ""))
  (assert (memeq? (base64.encode "A") "QQ=="))
  (assert (memeq? (base64.encode "AB") "QUI="))
  (assert (memeq? (base64.encode "ABC") "QUJD"))
  (assert (memeq? (base64.encode "ABCD") "QUJDRA=="))
  (assert (memeq? (base64.encode "ABCDE") "QUJDREU="))
  (assert (memeq? (base64.encode "ABCDEF") "QUJDREVG"))
  (assert (memeq? (base64.encode "ABCDEFG") "QUJDREVGRw=="))
  (assert (memeq? (base64.encode "abc123!?$*&()'-=@~") "YWJjMTIzIT8kKiYoKSctPUB+"))
  (assert (memeq? (base64.url-safe-encode "abc123!?$*&()'-=@~") "YWJjMTIzIT8kKiYoKSctPUB-"))
  (assert (memeq? (base64.decode (base64.encode "")) ""))
  (assert (memeq? (base64.decode (base64.encode "A")) "A"))
  (assert (memeq? (base64.decode (base64.encode "AB")) "AB"))
  (assert (memeq? (base64.decode (base64.encode "ABC")) "ABC"))
  (assert (memeq? (base64.decode (base64.encode "ABCD")) "ABCD"))
  (assert (memeq? (base64.decode (base64.encode "ABCDE")) "ABCDE"))
  (assert (memeq? (base64.decode (base64.encode "ABCDEF")) "ABCDEF"))
  (assert (memeq? (base64.decode (base64.encode "ABCDEFG")) "ABCDEFG"))
  (assert (memeq? (base64.decode (base64.encode "abc123!?$*&()'-=@~")) "abc123!?$*&()'-=@~"))
  (assert (memeq? (base64.decode (base64.url-safe-encode "abc123!?$*&()'-=@~")) "abc123!?$*&()'-=@~")))
