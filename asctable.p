; ASCII code table.

(function! main (args)
  ; Print ASCII code table.
  (write-line "   0 1 2 3 4 5 6 7 8 9 a b c d e f")
  (for (i 0) (<= i 0x7f) (<- i (++ i))
    (when (= (mod i 0x10) 0)
      (write-bytes (int->string i :radix 16 :padding 2)) (write-bytes " "))
    (write-byte (if (byte-print? i) i 0x2e))
    (write-bytes (if (= (mod i 0x10) 0x0f) "\n" " "))))
