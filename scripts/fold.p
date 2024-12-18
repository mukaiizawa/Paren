; fold.

(function fold (n)
  (let (ch nil i 0)
    (while (<- ch (read-char))
      (if (= ch "\n") (<- i 0)
          (> (<- i (+ i (wcwidth ch))) n)
          (begin
            (write-line)
            (<- i (wcwidth ch))))
      (write-bytes ch))))

(function! main (args)
  (fold (if args (int (car args)) 100)))
