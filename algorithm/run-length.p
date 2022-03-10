; Run-length encoding/decoding.

(function run-length.encode (s)
  (with-memory-stream ($out)
    (let (prev nil count nil)
      (doarray (ch s)
        (if (nil? prev) (<- prev ch count 1)
            (= ch prev) (<- count (++ count))
            (begin
              (write-bytes (str count prev))
              (<- prev ch count 1))))
      (if prev (write-bytes (str count prev))))))

(function run-length.decode (s)
  (with-memory-stream ($out)
    (let (length nil)
      (doarray (ch s)
        (if (nil? length) (<- length (int ch))
            (digit? ch) (<- length (+ (* length 10) (int ch))) 
            (begin
              (dotimes (i length) (write-bytes ch))
              (<- length nil)))))))

(function! main (args)
  (assert (= (run-length.encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW") "12W1B12W3B24W1B14W"))
  (assert (= (run-length.decode "12W1B12W3B24W1B14W")  "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")))
