; list directory contents.

(import :optparse)
(import :datetime)

(function ls (path :key long? recur? only-file? only-dir? full-path?)
  ; ls [OPTION]... [PATH]
  ; List information about the PATH (the current directory by default).
  ;     -l use long listing format
  ;     -r list subdirectories recursively
  ;     -f list only file
  ;     -d list only directories
  ;     -F use full path format
  (let (ls.d
         (f (path)
           (if (.dir? path) (foreach ls.f (.children path))))
         ls.f
         (f (path)
           (if (&& only-file? (! (.file? path))) (return nil)
               (&& only-dir? (! (.dir? path))) (return nil)
               (write1 path))
           (if recur? (ls.d path)))
         write1
         (f (path)
           (if long? (write-bytes (format "%s%s%s %11d %s "
                                          (if (.dir? path) "d" (.other? path) "?" "-")
                                          (if (.readable? path) "r" "-")
                                          (if (.writable? path) "w" "-")
                                          (.size path)
                                          (.to-s (.init (.new DateTime) (.mtime path))))))
           (write-line (if full-path? (.to-s path) (.name path)))))
    (ls.d path)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "lrfdF") args))
    (ls (path (|| (car args) "."))
        :long? (.get op "l")
        :recur? (.get op "r")
        :only-file? (.get op "f")
        :only-dir? (.get op "d")
        :full-path? (.get op "F"))))
