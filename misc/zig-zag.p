; zig-zag.

(function! main (args)
  (let (i 0 di 1)
    (loop
      (<- i (+ i di) di (if (= i 0) 1 (= i 15) -1 di))
      (write-line (format (str "%" (+ i 5) "s") "*****")))))
