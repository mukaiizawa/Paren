; basename.

(import :optparse)

(function basename (remove-suffix?)
  (dolist (file (map path (collect read-line)))
    (write-line (if (! remove-suffix?) (.name file) (.base-name file)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "s") args))
    (basename (.get op "s"))))
