; uri module.

(function url.encode (s)
  ; Encode the given string according to the percent-encode described in RFC3986.
  ; The caller must determine whether it should be encoded or not.
  (with-memory-stream ($out)
    (doarray (x (bytes s))
      (write-bytes (format "%%%02X" x)))))

(function url.decode (s)
  ; The given string is assumed to be encoded with percent-encode described in RFC3986 and decoded.
  ; The caller must determine whether it should be decoded or not.
  (let (trail? nil val nil digit-val (f (x) (index (upper x) "0123456789ABCDEF")))
    (with-memory-stream ($out)
      (dostring (ch s)
        (if (= ch "%") (<- trail? true)
            (nil? trail?) (write-bytes ch)
            (nil? val) (<- val (digit-val ch))
            (begin
              (write-byte (+ (* val 16) (digit-val ch)))
              (<- trail? nil val nil)))))))

(function! main (args)
  (assert (= (url.encode "012あ") "%30%31%32%E3%81%82"))
  (assert (= (url.decode "012%E3%81%82") "012あ"))
  (assert (= (url.decode "%30%31%32%E3%81%82") "012あ")))
