; strip directory and suffix from filenames.

(import :optparse)

(function basename (remove-suffix?)
  ; basename [OPTION]
  ; Write standard input to standard output, with any leading directory components removed.
  ;     -s remove suffix
  (foreach write-line
           (map (f (x)
                  (if (! remove-suffix?) (.name (path x))
                      (.base-name (path x))))
                (collect read-line))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "s") args))
    (basename (.get op "s"))))
