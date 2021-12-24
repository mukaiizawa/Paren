; sjis.

(function write-table (s e get-bytes cond)
  (for (i s digit 0) (<= i e) (i (++ i) digit (% i 0x10))
    (if (= digit 0) (write-bytes (format "0x%02x " i)))
    (if (cond i) (map write-byte (get-bytes i))
        (write-bytes "."))
    (if (= digit 0x0f) (write-byte 0x0a)
        (write-byte 0x20)))
  (write-line))

(function! main (args)
  (write-line "single-byte characters")
  (write-table 0x00 0xff
               (f (b1) (list b1))
               (f (b1) (|| (<= 0x20 b1 0x7e) (<= 0xa1 b1 0xdf))))
  (write-line "two-byte characters")
  (for (b1 0x81) (<= b1 0xfc) (b1 (++ b1))
    (when (|| (<= 0x81 b1 0x9f) (<= 0xe0 b1 0xfc))
      (write-line (hex (<< b1 8)))
      (write-table 0x40 0xfc
                   (f (b2) (list b1 b2))
                   (f (b2) true))
      (write-line))))
