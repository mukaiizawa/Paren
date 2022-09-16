; uniq.

(import :optparse)

(function! uniq (:key count? uniq? duplicate?)
  (let (n 1 prev (read-line)
              try-write (f (n line)
                          (when (|| (&& (! uniq?) (! duplicate?))
                                    (&& uniq? (= n 1))
                                    (&& duplicate? (!= n 1)))
                            (if count? (printf "%7d " n))
                            (println line))))
    (when prev
      (dolist (line (collect read-line))
        (if (= prev line) (<- n (++ n))
            (begin
              (try-write n prev)
              (<- n 1 prev line))))
      (try-write n prev))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "cdu") args))
    (uniq :count? (.get op "c")
          :uniq? (.get op "u")
          :duplicate? (.get op "d"))))
