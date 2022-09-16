; sjis.

(import :optparse)

(function write-table (s e get-bytes)
  (for (i s digit 0) (<= i e) (i (++ i) digit (% i 0x10))
    (if (= digit 0) (printf "0x%02x " i))
    (foreach write-byte (->list (get-bytes i)))
    (print (if (= digit 0x0f) "\n" " ")))
  (write-line))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "s") args))
    (write-line "single-byte characters")
    (write-table 0x00 0xff (f (b1) (if (|| (<= 0x20 b1 0x7e) (<= 0xa1 b1 0xdf)) b1 0x2e)))
    (when (! (.get op "s"))
      (write-line "two-byte characters")
      (for (b1 0x81) (<= b1 0xfc) (b1 (++ b1))
        (when (|| (<= 0x81 b1 0x9f) (<= 0xe0 b1 0xfc))
          (write-line (hex (<< b1 8)))
          (write-table 0x40 0xfc (f (b2) (list b1 b2)))
          (write-line))))))
