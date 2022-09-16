; display file contents in hexadecimal.

(import :optparse)

(function seek (size)
  (let (addr size n nil buf (bytes 4096))
    (while (!= (<- n (read-bytes buf 0 (min size (len buf)))) 0)
      (<- size (- size n)))
    addr))

(function hexdump (addr size hex?)
  (let (n nil buf (bytes 16))
    (while (!= (<- n (read-bytes buf 0 (min size (len buf)))) 0)
      ;; address
      (printf (if hex? "%7x" "%7d") addr)
      ;; contents
      (dotimes (i (min n (len buf)))
        (printf " %02x" ([] buf i)))
      (dotimes (i (++ (* (- (len buf) n) 3)))
        (printf " "))
      ;; character
      (dotimes (i n)
        (print (let (ch (chr ([] buf i))) (if (print? ch) ch "."))))
      (println)
      (<- size (- size n)
          addr (+ addr n)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:s:h") args))
    (hexdump (seek (.get-int op "s" 0)) (.get-int op "n" 256) (.get op "h"))))
