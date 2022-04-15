; file perusal filter for crt viewing.

(<- $line/page 24)

(function more ()
  ; more FILE
  ; Show prompto for each paging.
  ; Enter q at the prompt to exit the program.
  (dotimes (i $line/page)
    (let (line (read-line))
      (if (nil? line) (return true)
          (write-line line))))
  (write-bytes "more?")
  (if (!= (let ($in $stdin) (read-line)) "q") (more)))

(function! main (args)
  (if (nil? args) (raise ArgumentError)
      (with-open ($in (car args) :read)
        (more))))
