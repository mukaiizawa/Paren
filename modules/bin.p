; binary module.

;; Bit Operating

(function bin.rotr32 (i n)
  ; Returns the value obtained by rotating the two's complement binary representation of the specified int value right by the specified number of bits.
  (assert (<= n 32))
  (int32 (| (>> i n) (<< i (- 32 n)))))

(function bin.rotl32 (i n)
  ; Returns the value obtained by rotating the two's complement binary representation of the specified int value left by the specified number of bits.
  (assert (<= n 32))
  (int32 (| (<< i n) (>> i (- 32 n)))))

;; Endian Operating

(function bin.ui16->i16 (val)
  (if (> val 0x7fff) (- val 0x10000)
      val))

(function bin.i16->ui16 (val)
  (if (neg? val) (+ val 0x10000)
      val))

(function bin.ui32->i32 (val)
  (if (> val 0x7fffffff) (- val 0x100000000)
      val))

(function bin.i32->ui32 (val)
  (if (neg? val) (+ val 0x100000000)
      val))

;; Little Endian

(function bin.i16LE (buf offset)
  (bin.ui16->i16 (bin.ui16LE buf offset)))

(function bin.i16LE! (buf offset val)
  (bin.ui16LE! buf offset (bin.i16->ui16 val)))

(function bin.i32LE (buf offset)
  (bin.ui32->i32 (bin.ui32LE buf offset)))

(function bin.i32LE! (buf offset val)
  (bin.ui32LE! buf offset (bin.i32->ui32 val)))

(function bin.ui16LE (buf offset)
  (| ([] buf offset) (<< ([] buf (++ offset)) 8)))

(function bin.ui16LE! (buf offset val)
  (assert (<= 0 val 0xffff)) 
  ([] buf offset (& val 0xff))
  ([] buf (++ offset) (>> val 8))
  buf)

(function bin.ui32LE (buf offset)
  (| (bin.ui16LE buf offset) (<< (bin.ui16LE buf (+ offset 2)) 16)))

(function bin.ui32LE! (buf offset val)
  (bin.ui16LE! buf offset (& val 0xffff))
  (bin.ui16LE! buf (+ offset 2) (>> val 16)))

;; Big Endian

(function bin.i16BE (buf offset)
  (bin.ui16->i16 (bin.ui16BE buf offset)))

(function bin.i16BE! (buf offset val)
  (bin.ui16BE! buf offset (bin.i16->ui16 val)))

(function bin.i32BE (buf offset)
  (bin.ui32->i32 (bin.ui32BE buf offset)))

(function bin.i32BE! (buf offset val)
  (bin.ui32BE! buf offset (bin.i32->ui32 val)))

(function bin.ui16BE (buf offset)
  (| (<< ([] buf offset) 8) ([] buf (++ offset))))

(function bin.ui16BE! (buf offset val)
  (assert (<= 0 val 0xffff)) 
  ([] buf offset (>> val 8))
  ([] buf (++ offset) (& val 0xff))
  buf)

(function bin.ui32BE (buf offset)
  (| (<< (bin.ui16BE buf offset) 16) (bin.ui16BE buf (+ offset 2))))

(function bin.ui32BE! (buf offset val)
  (bin.ui16BE! buf offset (>> val 16))
  (bin.ui16BE! buf (+ offset 2) (& val 0xffff)))

;; Searching

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
  (let (buf #[ 0xff 0x00 0x11 0x8f 0x00 0x00 0x00 0x00 ])
    ;; le
    (assert (= (bin.i16->ui16 (bin.i16LE buf 0)) 0x00ff))
    (assert (= (bin.i16->ui16 (bin.i16LE (bin.i16LE! buf 4 0xfedc) 4)) 0xfedc))
    (assert (= (bin.i32->ui32 (bin.i32LE buf 0)) 0x8f1100ff))
    (assert (= (bin.i32->ui32 (bin.i32LE (bin.i32LE! buf 4 0xfedccdef) 4)) 0xfedccdef))
    (assert (= (bin.ui16LE buf 0) 0x00ff))
    (assert (= (bin.ui16LE (bin.ui16LE! buf 4 0xfedc) 4) 0xfedc))
    (assert (= (bin.ui32LE buf 0) 0x8f1100ff))
    (assert (= (bin.ui32LE (bin.ui32LE! buf 4 0xfedccdef) 4) 0xfedccdef))
    ;; be
    (assert (= (bin.i16->ui16 (bin.i16BE buf 0)) 0xff00))
    (assert (= (bin.i16->ui16 (bin.i16BE (bin.i16BE! buf 4 0xfedc) 4)) 0xfedc))
    (assert (= (bin.i32->ui32 (bin.i32BE buf 0)) 0xff00118f))
    (assert (= (bin.i32->ui32 (bin.i32BE (bin.i32BE! buf 4 0xfedccdef) 4)) 0xfedccdef))
    (assert (= (bin.ui16BE buf 0) 0xff00))
    (assert (= (bin.ui16BE (bin.ui16BE! buf 4 0xfedc) 4) 0xfedc))
    (assert (= (bin.ui32BE buf 0) 0xff00118f))
    (assert (= (bin.ui32BE (bin.ui32BE! buf 4 0xfedccdef) 4) 0xfedccdef)))
  (let (b #[ 0x00 0x01 ] c #[ 0x00 0x01 0x02 ])
    (assert (= (bin.search b 0x00) 0))
    (assert (= (bin.search b 0x01) 1))
    (assert (= (bin.search b 0x02) nil))
    (assert (= (bin.search c 0x00) 0))
    (assert (= (bin.search c 0x01) 1))
    (assert (= (bin.search c 0x02) 2))
    (assert (= (bin.search c 0x03) nil))))
