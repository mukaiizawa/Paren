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
  ; # NAME
  ; hexdump [OPTION]
  ;
  ; # DESCRIPTION
  ; The hexdump utility is a filter which displays the standard input in a hexdecimal format.
  ;
  ; # OPTION
  ;     -n N -- Dump for N bytes. The default is 256 bytes.
  ;     -s OFFSET -- Skip OFFSET bytes from the beginning of the input.
  ;     -h -- Print addresses in hexadecimal.
  (let ((op args) (.parse (.init (.new OptionParser) "n:s:h") args))
    (let (size (.get op "n") skip-size (int (.get op "s")))
      (<- size (if (nil? size) 256 (int size))
          $outline-hex? (.get op "h"))
      (skip skip-size)
      (hexdump skip-size size))))