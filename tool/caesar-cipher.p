; Caesar's cipher.

(function shift (byte n)
  (let (base (ord (if (upper? (chr byte)) "A" "a")))
    (+ (% (+ (- byte base) n) 26) base)))

(function caesar-cipher (n)
  (let (byte nil)
    (while (!= (<- byte (read-byte)) -1)
      (write-byte (if (alpha? (chr byte)) (shift byte n) byte)))))

(function! main (args)
  (caesar-cipher 13))
