; binary module.

(function bin.rotr32 (i n)
  ; Returns the value obtained by rotating the two's complement binary representation of the specified int value right by the specified number of bits.
  (assert (<= n 32))
  (int32 (| (>> i n) (<< i (- 32 n)))))

(function bin.rotl32 (i n)
  ; Returns the value obtained by rotating the two's complement binary representation of the specified int value left by the specified number of bits.
  (assert (<= n 32))
  (int32 (| (<< i n) (>> i (- 32 n)))))

(function bin.search (seq x :opt start end)
  ; If it is not sorted, the results are undefined.
  ; If the array contains multiple elements equal to the specified object, there is no guarantee which one will be found.
  (let (lo (|| start 0) hi (-- (|| end (len seq))))
    (while (<= lo hi)
      (let (mi (>> (+ lo hi) 1) y ([] seq mi))
        (if (= x y) (return mi)
            (< x y) (<- hi (-- mi))
            (<- lo (++ mi)))))))

(function! main (args)
  (assert (= (bin.rotr32 0xfffffffe 1) 0x7fffffff))
  (assert (= (bin.rotl32 0x7fffffff 1) 0xfffffffe))
  (assert (= (bin.search #[ 0x00 0x01 ] 0x00) 0))
  (assert (= (bin.search #[ 0x00 0x01 ] 0x01) 1))
  (assert (= (bin.search #[ 0x00 0x01 ] 0x02) nil))
  (assert (= (bin.search #[ 0x00 0x01 0x02 ] 0x00) 0))
  (assert (= (bin.search #[ 0x00 0x01 0x02 ] 0x01) 1))
  (assert (= (bin.search #[ 0x00 0x01 0x02 ] 0x02) 2))
  (assert (= (bin.search #[ 0x00 0x01 0x02 ] 0x03) nil)))
