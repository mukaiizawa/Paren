; crc32 check sum.

(import :crc32)

(function! main (args)
  (write (crc32.sum (read-bytes))))
