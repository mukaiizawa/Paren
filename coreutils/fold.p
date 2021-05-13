; wrap each input line to fit in specified width.

(function fold (n)
  ; fold [WIDTH]
  ; Wrap standard input lines, writing to standard output.
  ; Number WIDTH columns instead of 100.
  (let (ch nil i 0)
    (while (<- ch (read-char))
      (if (= ch "\n") (<- i 0)
          (> (<- i (+ i (wcwidth ch))) n)
          (begin
            (write-line)
            (<- i (wcwidth ch))))
      (write-bytes ch))))

(function! main (args)
  (fold (if args (str->num (car args)) 100)))
