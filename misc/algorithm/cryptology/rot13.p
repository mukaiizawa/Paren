; ROT13.

(import :caesar-cipher "./")

(function! main (args)
  (write-bytes (caesar-cipher 13 (read-bytes))))
