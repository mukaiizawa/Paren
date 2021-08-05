; outline.

(import :optparse)
(import :re)

(function! main (args)
  (let ((op args :opt line) (.parse (.init (.new OptionParser) "l:") args)
                            level (int (|| (.get op "l") 6))
                            expr (re.compile (str "^#{1," level "} ")))
    (while (<- line (read-line))
      (if (re.match expr line) (write-line line)))))
