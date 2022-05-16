; display file contents in hexadecimal.

(import :optparse)

(<- $outline-hex? nil)

(function write1 (addr buf size)
  (write-bytes (format (if $outline-hex? "0x%05x " "% 7d ") addr))
  (dotimes (i (len buf))
    (write-bytes (if (< i size) (format "%02x " ([] buf i)) "   ")))
  (dotimes (i size)
    (write-bytes (let (ch (chr ([] buf i))) (if (print? ch) ch "."))))
  (write-line))

(function skip (size)
  (let (n nil buf (bytes 4096))
    (while (!= (<- n (read-bytes buf 0 (min size (len buf)))) 0)
      (<- size (- size n)))))

(function hexdump (addr size)
  (let (n nil buf (bytes 16))
    (while (!= (<- n (read-bytes buf 0 (min size (len buf)))) 0)
      (write1 addr buf n)
      (<- size (- size n)
          addr (+ addr n)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:s:h") args))
    (let (size (.get-int op "n" 256) skip-size (.get-int op "s" 0))
      (<- $outline-hex? (.get op "h"))
      (skip skip-size)
      (hexdump skip-size size))))
