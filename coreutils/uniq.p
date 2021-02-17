; report or omit repeated lines.

(import :optparse)

(function uniq (:key count? uniq? duplicate?)
  ; uniq [OPTION]...
  ; Filter adjacent matching lines from standard input, writing to standard output.
  ;     -c prefix lines by the number of occurrences
  ;     -d only print duplicate lines
  ;     -u only print unique lines
  (let (count 1 prev (read-line)
              try-write (f (count line)
                          (when (|| (&& (! uniq?) (! duplicate?))
                                    (&& uniq? (= c 1))
                                    (&& duplicate? (/= c 1)))
                            (when count?
                              (write-mem (int->str count :padding 7))
                              (write-mem " "))
                            (write-line line))))
    (when prev
      (dolist (line (collect read-line))
        (if (memeq? prev line) (<- count (++ count))
            (begin
              (try-write count prev)
              (<- count 1 prev line))))
      (try-write count prev))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "cdu") args))
    (uniq :count? (.get op "c")
          :uniq? (.get op "u")
          :duplicate? (.get op "d"))))
