; print newline, word, and byte counts of standard input.

(import :optparse)

(function wc ()
  ; wc [OPTION]...
  ; Print newline, word, and byte counts.
  ;     -b print the byte counts
  ;     -w print the word counts
  ;     -l print the line counts
  (let (c nil bytec 0 wordc 0 linec 0)
    (while (<- c (read-char))
      (if (= c "\n") (<- linec (++ linec))
          (= c " ") (<- wordc (++ wordc)))
      (<- bytec (+ bytec (len c))))
    (list bytec wordc linec)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "bwl") args)
                  (bytec wordc linec) (wc))
    (if (.get op "b") (write bytec)
        (.get op "w") (write wordc)
        (.get op "l") (write linec)
        (write-line (join (map str (list bytec wordc linec)) " ")))))
