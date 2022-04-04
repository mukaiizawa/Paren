; Vigenère cipher.

(import :optparse)

(<- $table nil)

(function ->i (alpha)
  (- (ord (upper alpha)) 0x41))

(function ->alpha (i)
  (chr (+ i 0x41)))

(function rot (lis n)
  (concat (slice lis n) (slice lis 0 n)))

(function make-row (i)
  (rot (map ->alpha (.. 26)) i))

(function make-table ()
  (map make-row (.. 26)))

(function lookup (row :opt col)
  (if (nil? $table) (<- $table (make-table)))
  (let (row ([] $table (->i row)))
    (if (nil? col) row
        ([] row (->i col)))))

(function vigenere-cipher (key text decrypt?)
  (with-memory-stream ($out)
    (dotimes (i (len text))
      (let (text-ch (chr ([] text i)) key-ch (chr ([] key (% i (len key)))))
        (write-bytes (if (! (alpha? text-ch)) text-ch
                         (! decrypt?) (lookup key-ch text-ch)
                         (->alpha (index text-ch (lookup key-ch)))))))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "d") args)
                  key (bytes (car args)) text (read-bytes))
    (write-bytes (vigenere-cipher key text (.get op "d")))))
