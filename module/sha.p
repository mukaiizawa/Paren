; sha module.

(import :bin)

(<- sha256.table
    #[ 0x428a2f98 0x71374491 0xb5c0fbcf 0xe9b5dba5 0x3956c25b 0x59f111f1 0x923f82a4 0xab1c5ed5
       0xd807aa98 0x12835b01 0x243185be 0x550c7dc3 0x72be5d74 0x80deb1fe 0x9bdc06a7 0xc19bf174
       0xe49b69c1 0xefbe4786 0x0fc19dc6 0x240ca1cc 0x2de92c6f 0x4a7484aa 0x5cb0a9dc 0x76f988da
       0x983e5152 0xa831c66d 0xb00327c8 0xbf597fc7 0xc6e00bf3 0xd5a79147 0x06ca6351 0x14292967
       0x27b70a85 0x2e1b2138 0x4d2c6dfc 0x53380d13 0x650a7354 0x766a0abb 0x81c2c92e 0x92722c85
       0xa2bfe8a1 0xa81a664b 0xc24b8b70 0xc76c51a3 0xd192e819 0xd6990624 0xf40e3585 0x106aa070
       0x19a4c116 0x1e376c08 0x2748774c 0x34b0bcb5 0x391c0cb3 0x4ed8aa4a 0x5b9cca4f 0x682e6ff3
       0x748f82ee 0x78a5636f 0x84c87814 0x8cc70208 0x90befffa 0xa4506ceb 0xbef9a3f7 0xc67178f2 ])

(function sha256.next (W H1 H2 H3 H4 H5 H6 H7 H8)
  (let ((h1 h2 h3 h4 h5 h6 h7 h8) (list H1 H2 H3 H4 H5 H6 H7 H8)
                                  T1 nil T2 nil K sha256.table
                                  Ch (f (x y z) (^ (& x y) (& (~ x) z)))
                                  Maj (f (x y z) (^ (& x y) (& x z) (& y z)))
                                  sig0 (f (x) (^ (bin.rotr32 x 2) (bin.rotr32 x 13) (bin.rotr32 x 22)))
                                  sig1 (f (x) (^ (bin.rotr32 x 6) (bin.rotr32 x 11) (bin.rotr32 x 25))))
    (dotimes (i 64)
      (<- T1 (bin.&32 (+ h8 (sig1 h5) (Ch h5 h6 h7) ([] K i) ([] W i)))
          T2 (bin.&32 (+ (sig0 h1) (Maj h1 h2 h3)))
          h8 h7 h7 h6 h6 h5 h5 (bin.&32 (+ h4 T1))
          h4 h3 h3 h2 h2 h1 h1 (bin.&32 (+ T1 T2))))
    (map bin.&32 (list (+ H1 h1)
                       (+ H2 h2)
                       (+ H3 h3)
                       (+ H4 h4)
                       (+ H5 h5)
                       (+ H6 h6)
                       (+ H7 h7)
                       (+ H8 h8)))))

(function sha256.W (n msg msglen padding)
  (let (W (array 64)
          at (f (i) (if (< i msglen) ([] msg i)
                        ([] padding (- i msglen))))
          sig0 (f (x) (^ (bin.rotr32 x 7) (bin.rotr32 x 18) (>> x 3)))
          sig1 (f (x) (^ (bin.rotr32 x 17) (bin.rotr32 x 19) (>> x 10))))
    (dotimes (i 64)
      (if (< i 16) (let (p (+ (* n 64) (* i 4)))
                     ([] W i (| (<< (at p) 24)
                                (<< (at (+ p 1)) 16)
                                (<< (at (+ p 2)) 8)
                                (at (+ p 3)))))
          ([] W i (bin.&32 (+ ([] W (- i 7)) ([] W (- i 16))
                              (sig0 ([] W (- i 15))) (sig1 ([] W (- i 2))))))))
    W))

(function sha256.sum (msg)
  ; SHA256 hash algorithm as defined in FIPS 180-4.
  ; Returns the SHA256 checksum of the data.
  (let ((H1 H2 H3 H4 H5 H6 H7 H8)
        '(0x6a09e667 0xbb67ae85 0x3c6ef372 0xa54ff53a
          0x510e527f 0x9b05688c 0x1f83d9ab 0x5be0cd19)
        msglen (memlen msg)
        N (++ (>> (+ msglen 8) 6))
        padlen (- (<< N 6) msglen)
        padding (bytes padlen)
        sha256sum (bytes 32))
    ([] padding 0 0x80)
    (for (i 0 len-bits (<< msglen 3)) (> len-bits 0) (i (++ i) len-bits (>> len-bits 8))
      ([] padding (- padlen i 1) (& 0xff len-bits)))
    (dotimes (n N)
      (let (W (sha256.W n msg msglen padding))
        (<- (H1 H2 H3 H4 H5 H6 H7 H8) (sha256.next W H1 H2 H3 H4 H5 H6 H7 H8))))
    (dotimes (i 8)
      (let (x (nth i (list H1 H2 H3 H4 H5 H6 H7 H8)))
        ([] sha256sum (+ (* i 4) 3) (& x 0xff))
        ([] sha256sum (+ (* i 4) 2) (& (>> x 8) 0xff))
        ([] sha256sum (+ (* i 4) 1) (& (>> x 16) 0xff))
        ([] sha256sum (* i 4)       (& (>> x 24) 0xff))))
    sha256sum))

(function! main (args)
  (assert (= (bin.hexstr (sha256.sum "")) "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
  (assert (= (bin.hexstr (sha256.sum "d3")) "f451a61749c611ba0fa0e16c61831db44f38c611dff25879cf271a24c81a88b6"))
  (assert (= (bin.hexstr (sha256.sum "11af")) "5e768653556b74ba4e447d51f3f234eb70006a75a9faf8b0e487603f31ad4cf8"))
  (assert (= (bin.hexstr (sha256.sum "451101250ec6f26652249d59dc974b7361d571a8101cdfd36aba3b5854d3ae086b5fdd4597721b66e3c0dc5d8c606d9657d0e323283a5217d1f53f2f284f57b85c8a61ac8924711f895c5ed90ef17745ed2d728abd22a5f7a13479a462d71b56c19a74a40b655c58edfe0a188ad2cf46cbf30524f65d423c837dd1ff2bf462ac4198007345bb44dbb7b1c861298cdf61982a833afc728fae1eda2f87aa2c9480858bec")) "ca35b7786f805d461b07cb1594d9cb17104c8b3a1ab167f4b2507337c67df636")))
