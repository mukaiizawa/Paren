; nl.

(import :optparse)

(function nl (:opt start :key csv?)
  (let (n (if start (int start) 1))
    (dolist (line (collect read-line))
      (if (nil? csv?) (printf "%7d %s\n" n line)
          (println n "," line))
      (<- n (++ n)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "c") args))
    (nl (car args) :csv? (.get op "c"))))
