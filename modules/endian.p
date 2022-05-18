; endian module.

(function endian.ui16->i16 (val)
  (if (> val 0x7fff) (- val 0x10000)
      val))

(function endian.i16->ui16 (val)
  (if (neg? val) (+ val 0x10000)
      val))

(function endian.ui32->i32 (val)
  (if (> val 0x7fffffff) (- val 0x100000000)
      val))

(function endian.i32->ui32 (val)
  (if (neg? val) (+ val 0x100000000)
      val))

;; Little Endian.

(function endian.i16LE (buf offset)
  (endian.ui16->i16 (endian.ui16LE buf offset)))

(function endian.i16LE! (buf offset val)
  (endian.ui16LE! buf offset (endian.i16->ui16 val)))

(function endian.i32LE (buf offset)
  (endian.ui32->i32 (endian.ui32LE buf offset)))

(function endian.i32LE! (buf offset val)
  (endian.ui32LE! buf offset (endian.i32->ui32 val)))

(function endian.ui16LE (buf offset)
  (| ([] buf offset) (<< ([] buf (++ offset)) 8)))

(function endian.ui16LE! (buf offset val)
  (assert (<= 0 val 0xffff)) 
  ([] buf offset (& val 0xff))
  ([] buf (++ offset) (>> val 8))
  buf)

(function endian.ui32LE (buf offset)
  (| (endian.ui16LE buf offset) (<< (endian.ui16LE buf (+ offset 2)) 16)))

(function endian.ui32LE! (buf offset val)
  (endian.ui16LE! buf offset (& val 0xffff))
  (endian.ui16LE! buf (+ offset 2) (>> val 16)))

;; Big Endian.

(function endian.i16BE (buf offset)
  (endian.ui16->i16 (endian.ui16BE buf offset)))

(function endian.i16BE! (buf offset val)
  (endian.ui16BE! buf offset (endian.i16->ui16 val)))

(function endian.i32BE (buf offset)
  (endian.ui32->i32 (endian.ui32BE buf offset)))

(function endian.i32BE! (buf offset val)
  (endian.ui32BE! buf offset (endian.i32->ui32 val)))

(function endian.ui16BE (buf offset)
  (| (<< ([] buf offset) 8) ([] buf (++ offset))))

(function endian.ui16BE! (buf offset val)
  (assert (<= 0 val 0xffff)) 
  ([] buf offset (>> val 8))
  ([] buf (++ offset) (& val 0xff))
  buf)

(function endian.ui32BE (buf offset)
  (| (<< (endian.ui16BE buf offset) 16) (endian.ui16BE buf (+ offset 2))))

(function endian.ui32BE! (buf offset val)
  (endian.ui16BE! buf offset (>> val 16))
  (endian.ui16BE! buf (+ offset 2) (& val 0xffff)))

(function! main (args)
  (let (buf #[ 0xff 0x00 0x11 0x8f 0x00 0x00 0x00 0x00 ])
    ;; le
    (assert (= (endian.i16->ui16 (endian.i16LE buf 0)) 0x00ff))
    (assert (= (endian.i16->ui16 (endian.i16LE (endian.i16LE! buf 4 0xfedc) 4)) 0xfedc))
    (assert (= (endian.i32->ui32 (endian.i32LE buf 0)) 0x8f1100ff))
    (assert (= (endian.i32->ui32 (endian.i32LE (endian.i32LE! buf 4 0xfedccdef) 4)) 0xfedccdef))
    (assert (= (endian.ui16LE buf 0) 0x00ff))
    (assert (= (endian.ui16LE (endian.ui16LE! buf 4 0xfedc) 4) 0xfedc))
    (assert (= (endian.ui32LE buf 0) 0x8f1100ff))
    (assert (= (endian.ui32LE (endian.ui32LE! buf 4 0xfedccdef) 4) 0xfedccdef))
    ;; be
    (assert (= (endian.i16->ui16 (endian.i16BE buf 0)) 0xff00))
    (assert (= (endian.i16->ui16 (endian.i16BE (endian.i16BE! buf 4 0xfedc) 4)) 0xfedc))
    (assert (= (endian.i32->ui32 (endian.i32BE buf 0)) 0xff00118f))
    (assert (= (endian.i32->ui32 (endian.i32BE (endian.i32BE! buf 4 0xfedccdef) 4)) 0xfedccdef))
    (assert (= (endian.ui16BE buf 0) 0xff00))
    (assert (= (endian.ui16BE (endian.ui16BE! buf 4 0xfedc) 4) 0xfedc))
    (assert (= (endian.ui32BE buf 0) 0xff00118f))
    (assert (= (endian.ui32BE (endian.ui32BE! buf 4 0xfedccdef) 4) 0xfedccdef))))
