; binary module.

(function bin.hexstr (mem)
  ; Returns the hexadecimal representation of the byte sequence.
  (with-memory-stream ($out)
    (doarray (x mem) (write-bytes (int->str x :radix 16 :padding 2)))))

(function bin.&32 (i)
  ; Returns a value with the argument masked in 32 bits.
  (& i 0xffffffff))

(function bin.rotr32 (i n)
  ; Returns the value obtained by rotating the two's complement binary representation of the specified int value right by the specified number of bits.
  (assert (<= n 32))
  (| (>> i n) (<< i (- 32 n))))

(function bin.rotl32 (i n)
  ; Returns the value obtained by rotating the two's complement binary representation of the specified int value left by the specified number of bits.
  (assert (<= n 32))
  (| (<< i n) (>> i (- 32 n))))

(function! main (args)
  (assert (= (bin.hexstr (bytes 3)) "000000")))
