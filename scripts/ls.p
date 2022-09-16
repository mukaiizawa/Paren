; ls.

(import :optparse)
(import :datetime)

(function ls (file :key long? recur? only-file? only-dir? full-path?)
  (let (ls.d
         (f (file)
           (if (.dir? file) (foreach ls.f (.children file))))
         ls.f
         (f (file)
           (if (&& only-file? (! (.file? file))) (return nil)
               (&& only-dir? (! (.dir? file))) (return nil)
               (write1 file))
           (if recur? (ls.d file)))
         write1
         (f (file)
           (if long? (printf "%s%s%s %11d %s "
                             (if (.dir? file) "d" (.other? file) "?" "-")
                             (if (.readable? file) "r" "-")
                             (if (.writable? file) "w" "-")
                             (.size file)
                             (.to-s (.init (.new DateTime) (.mtime file)))))
           (println (if full-path? (.to-s file) (.name file)))))
    (ls.d file)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "lrfdF") args))
    (ls (path (|| (car args) "."))
        :long? (.get op "l")
        :recur? (.get op "r")
        :only-file? (.get op "f")
        :only-dir? (.get op "d")
        :full-path? (.get op "F"))))
