; internationalization conversion.

(import :iconv)
(import :optparse)

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "l") args))
    (if (.get op "l") (foreach (f (x) (println (slice (str x) 1)))
                               $iconv.encodings)
        (!= (len args) 2) (raise ArgumentError "invalid arguments")
        (write-bytes (apply iconv.encode
                            (cons (string (read-bytes))
                                  (map keyword args)))))))
