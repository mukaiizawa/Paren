; file perusal filter for crt viewing.

(<- $line/page 24)

(function! main (args)
  (if (nil? args) (raise ArgumentError "require file argument")
      (with-open ($in (car args) :read)
        (loop
          (dotimes (i $line/page)
            (let (line (read-line))
              (if (nil? line) (return true)
                  (write-line line))))
          (write-bytes "more?")
          (let ($in $stdin)
            (if (prefix? (read-line) "q") (return true)))))))
