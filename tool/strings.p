;  print the sequences of printable characters in files.

(import :optparse)

(function strings (:opt n)
  ; strings [OPTION]
  ; Strings prints the printable character sequences that are at least 4 characters long and are followed by an unprintable character.
  ;     -n print sequences of characters that are at least min-len characters long.
  (let (n (if n (str->num n) 4) byte 0 ms (.new MemoryStream)
          flush (f ()
                  (if (>= (.size ms) n) (write-line (.to-s ms)))
                  (.reset ms)))
    (while (!= (<- byte (read-byte)) -1)
      (if (print? (chr byte)) (.write-byte ms byte)
          (flush)))
    (flush)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "n:") args))
    (strings (.get op "n"))))
