; zipinfo.

(import :optparse)
(import :zip)

(function zipinfo ()
  (let (rd (.new ZipReader))
    (dolist (entry (collect (f () (.read rd))))
      (let (usize (.uncompressed-size entry)
                  csize (.compressed-size entry)
                  name (.file-name entry)
                  timestamp (.to-s (.timestamp entry)))
        (if (pos? usize) (write-line (format "%11d %11d %3d%% %s %s" usize csize (* 100 (/ csize usize)) timestamp name)))
        (if $verbose? (write entry))))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "v") args))
    (<- $verbose? (.get op "v"))
    (if (nil? args) (zipinfo)
        (foreach (f (x) (with-open ($in x :read) (zipinfo)))
                 args))))
