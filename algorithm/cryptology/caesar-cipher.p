; Caesar's cipher.

(function shift (byte n)
  (if (! (alpha? (chr byte))) byte
      (let (base (ord (if (upper? (chr byte)) "A" "a")))
        (+ (% (+ (- byte base) n) 26) base))))

(function caesar-cipher (n text)
  (with-memory-stream ($out)
    (dotimes (i (len text))
      (write-byte (shift ([] text i) n)))))

(function! main (args)
  (write-bytes (caesar-cipher (int (car args)) (read-bytes))))
