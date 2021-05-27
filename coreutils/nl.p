; number lines of files.

(import :optparse)

(function nl (:opt start :key csv?)
  ; nl [OPTION] [START]
  ; Write standard input to standard output, with line numbers starting from START added.
  ; If the START is omitted, it is considered that the 1 is specified.
  ;     -n consider the input as CSV
  (let (n (if start (int start) 1))
    (dolist (line (collect read-line))
      (if csv?
          (write-line (str n "," line))
          (write-line (format "%-7d %s" n line)))
      (<- n (++ n)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "c") args))
    (nl (car args) :csv? (.get op "c"))))
