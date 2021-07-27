; summarize disk usage.

(import :optparse)

(function du (dir :key all? summary?)
  (let (sweep (f (files level)
                ;; Returns files size.
                (apply + (map (f (file)
                                (if (.dir? file) (print file (sweep (.children file) (++ level)) level)
                                    (let (size (.size file))
                                      (if (nil? size) (raise OSError (str "illegal file " (.to-s file)))
                                          all? (print file size level)
                                          size))))
                              files)))
              print (f (file size level)
                      ;; Returns size.
                      (if (! (&& summary? (!= level 0)))
                          (write-line (format "%11d %s" size (.to-s file))))
                      size))
    (print dir (sweep (.children dir) 0) 0)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "as") args))
    (du (path (if args (car args) "."))
        :all? (.get op "a")
        :summary? (.get op "s"))))
