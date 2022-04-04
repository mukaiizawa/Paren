; wc.

(import :optparse)

(function ++? (test n)
  (if (! test) n (++ n)))

(function word-count ()
  (let (ch (read-char) in-space? (&& ch (space? ch)) (lc wc cc bc) '(0 0 0 0))
    (while ch
      (<- bc (+ bc (byte-len ch))
          cc (++ cc)
          wc (++? (&& (space? ch) (! in-space?)) wc)
          lc (++? (= ch "\n") lc)
          in-space? (space? ch)
          ch (read-char)))
    (list lc (++? (! in-space?) wc) cc bc)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "bclw") args)
                  (lc? wc? cc? bc?) (list (.get op "l") (.get op "w") (.get op "c") (.get op "b"))
                  (lc wc cc bc) (word-count))
    (if (|| lc? wc? cc? bc?)
        (begin
          (if lc? (write lc :end " "))
          (if wc? (write wc :end " "))
          (if cc? (write cc :end " "))
          (if bc? (write bc :end " ")))
        (write-line (format "%d %d %d" lc wc bc)))))
