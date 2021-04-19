; binary module.

(function hexstr (mem)
  ; Returns the hexadecimal representation of the byte sequence.
  (with-memory-stream ($out)
    (dotimes (i (memlen mem))
      (write-mem (int->str ([] mem i) :radix 16 :padding 2)))))

(function i32 (i)
  ; Returns a value with the argument masked in 32 bits.
  (& i 0xffffffff))

(function i32rotr (i n)
  ; Returns the value obtained by rotating the two's complement binary representation of the specified int value right by the specified number of bits.
  (assert (<= n 32))
  (| (>> i n) (<< i (- 32 n))))

(function i32rotl (i n)
  ; Returns the value obtained by rotating the two's complement binary representation of the specified int value left by the specified number of bits.
  (assert (<= n 32))
  (| (<< i n) (>> i (- 32 n))))

(function! main (args)
  (assert (= (hexstr (bytes 3)) "000000"))
  (assert (= (i32 (i32rotr 2x110011 2)) 2x111100)))
