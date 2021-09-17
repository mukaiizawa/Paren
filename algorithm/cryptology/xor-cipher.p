; XOR cipher.

(function xor-cipher (key text)
  (with-memory-stream ($out)
    (dotimes (i (len text))
      (write-byte (^ ([] text i) ([] key (% i (len key))))))))

(function! main (args)
  (write-bytes (xor-cipher (bytes (car args)) (read-bytes))))
