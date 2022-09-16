; zipinfo.

(import :optparse)
(import :zip)

(function zipinfo ()
  (dolist (entry (collect (partial .read (.new ZipReader))))
    (let (usize (.uncompressed-size entry)
                csize (.compressed-size entry)
                name (.file-name entry)
                timestamp (.to-s (.timestamp entry)))
      (if (pos? usize) (printf "%11d %11d %3d%% %s %s\n" usize csize (* 100 (/ csize usize)) timestamp name))
      (if $verbose? (print entry)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "v") args))
    (<- $verbose? (.get op "v"))
    (if (nil? args) (zipinfo)
        (foreach (f (x) (with-open ($in x :read) (zipinfo)))
                 args))))
