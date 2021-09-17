; XOR cipher.

(function xor-cipher (key text)
  (dotimes (i (len text))
    (write-byte (^ ([] text i) ([] key (% i (len key)))))))

(function! main (args)
  (xor-cipher (bytes (car args)) (read-bytes)))
