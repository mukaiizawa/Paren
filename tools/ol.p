; outline.

(import :optparse)
(import :re)

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "l:") args)
                  level (int (|| (.get op "l") 6))
                  expr (re.compile (str "^#{1," level "} ")))
    (foreach write-line
             (select (f (x) (re.match expr x))
                     (collect read-line)))))
