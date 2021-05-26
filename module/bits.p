; bits module.

(function bits.rotr32 (i n)
  ; Returns the value obtained by rotating the two's complement binary representation of the specified int value right by the specified number of bits.
  (assert (<= n 32))
  (int32 (| (>> i n) (<< i (- 32 n)))))

(function bits.rotl32 (i n)
  ; Returns the value obtained by rotating the two's complement binary representation of the specified int value left by the specified number of bits.
  (assert (<= n 32))
  (int32 (| (<< i n) (>> i (- 32 n)))))

(function! main (args)
  (assert (= (bits.rotr32 0xfffffffe 1) 0x7fffffff))
  (assert (= (bits.rotl32 0x7fffffff 1) 0xfffffffe)))
