;  strings.

(import :optparse)

(function strings (:opt n)
  (let (byte 0 ms (.new MemoryStream)
          flush (f ()
                  (if (>= (.size ms) n) (write-line (.to-s ms)))
                  (.reset ms)))
    (while (!= (<- byte (read-byte)) -1)
      (if (print? (chr byte)) (.write-byte ms byte)
          (flush)))
    (flush)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args))
    (strings (.get-int op "n" 4))))
