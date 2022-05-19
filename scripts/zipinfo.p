; zipinfo.

(import :optparse)
(import :zip)

(function zipinfo ()
  (dolist (entry (.entries (.read (.new Zip))))
    (write-line (format "%11d %11d %3d%% %s %s"
                        (.uncompressed-size entry)
                        (.compressed-size entry)
                        (* 100 (/ (.compressed-size entry) (.uncompressed-size entry)))
                        (.file-name entry)
                        (.to-s (.timestamp entry))))
    (if $verbose? (write entry))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "v") args))
    (<- $verbose? (.get op "v"))
    (if (nil? args) (zipinfo)
        (foreach (f (x) (with-open ($in x :read) (zipinfo)))
                 args))))
