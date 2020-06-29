; Base64 module.
; base64 encoding as specified by RFC 4648.

; # input
;     ABCDEFG
;
; # radix 16
;     0100 0001 0100 0010 0100 0011 0100 0100 0100 0101 0100 0110 0100 0111
;
; # split 6bit
;     010000 010100 001001 000011 010001 000100 010101 000110 010001 11
;
; # align
;     010000 010100 001001 000011 010001 000100 010101 000110 010001 110000
;
; # convert each 4 bytes.
;     QUJD REVG Rw
;
; # remain
;     QUJD REVG Rw==
;
; # result
;     QUJDREVGRw==

(global-symbol Base64.mapping (->byte-array "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))

(class Base64 ())
(class Base64.encoder ())
(class Base64.decoder ())

(function Base64.encode (src))
(function Base64.decode (src))

(function Base64.url-safe-encode (src))
(function Base64.url-safe-decode (src))

(method Base64 encode (src)
  ; Encodes byte-array (or string) src.
  (let (src (->byte-array src) di 0 si 0 n (* (// (length src) 3) 3))
    (with-memory-stream (dst)
      (while (< si n)
        (let (val (nth src si)
                  (nth src si)
                  (nth src si)
                  (nth src si))
          val (<- n (++ n))

              ; todo see test.go

)

(method Base64 decode (s)
  (error "not yet implementd"))

(function main ()
  ; test cases.
  (error "not yet implementd"))
