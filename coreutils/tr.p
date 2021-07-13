; translate or delete characters.

(function expand-hyphen (s)
  (with-memory-stream ($out)
    (with-memory-stream ($in s)
      (let (rd (.new AheadReader) ch nil)
        (while (.next rd)
          (write-bytes (<- ch (.skip-escape rd)))
          (if (= (.next rd) "-")
              (let (ordch (ord ch))
                (.skip rd)
                (dotimes (i (- (ord (.skip-escape rd)) ordch))
                  (write-bytes (chr (+ ordch i 1)))))))))))

(function tr (src :opt dst)
  ; tr SET1 [SET2]
  ; Translate, squeeze, and/or delete characters from standard input, writing to standard output.
  ; SETs are specified as strings of characters.
  ;     CHAR1-CHAR2 -- all characters from CHAR1 to CHAR2 in ascending order
  (let (src (expand-hyphen src)
            dst (if dst (expand-hyphen dst) "")
            table (dict))
    (dotimes (i (len src))
      ([] table ([] src i) (if (< i (len dst)) ([] dst i) "")))
    (let (ch nil)
      (while (<- ch (read-char))
        (write-bytes (|| ([] table ch) ch))))))

(function! main (args)
  (let ((src :opt dst) args)
    (tr src dst)))
