; uniq.

(import :optparse)

(function uniq (:key count? uniq? duplicate?)
  (let (count 1 prev (read-line)
              try-write (f (count line)
                          (when (|| (&& (! uniq?) (! duplicate?))
                                    (&& uniq? (= count 1))
                                    (&& duplicate? (!= count 1)))
                            (if count? (write-bytes (format "%7d " count)))
                            (write-line line))))
    (when prev
      (dolist (line (collect read-line))
        (if (= prev line) (<- count (++ count))
            (begin
              (try-write count prev)
              (<- count 1 prev line))))
      (try-write count prev))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "cdu") args))
    (uniq :count? (.get op "c")
          :uniq? (.get op "u")
          :duplicate? (.get op "d"))))
