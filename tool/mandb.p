; mandb.

(import :man (.resolve $paren-home "tool"))

(function man-dir? (dir)
  (&& (.dir? dir) (prefix? (.name dir) "man")))

(function man-dir->section (dir)
  (slice (.name dir) 3))

(function man-file? (file)
  (&& (.file? file) (= (.suffix file) "md")))

(function man-header? (line)
  (prefix? line "# NAME"))

(function make-index (dir file)
  ;; page [', ' page]... ' - ' one-line-desc
  (with-open ($in file :read)
    (let (rd (.new AheadReader))
      (if (! (man-header? (.skip-line rd))) (raise ArgumentError "illegal manual file")
          (let (pages nil)
            (loop
              (if (= (.next rd) " ")
                  (begin
                    (push! (.token rd) pages)
                    (.skip rd " ") (.skip rd "-") (.skip rd " ")
                    (let (one-line-desc (.skip-line rd))
                      (<- pages (reverse! pages))
                      (return (list pages one-line-desc (.to-s file)))))
                  (= (.next rd) ",")
                  (begin
                    (.skip rd ",")
                    (if (= (.next rd) " ")
                        (begin
                          (.skip rd)
                          (push! (.token rd) pages))
                        (.put rd ",")))
                  (.get rd))))))))

(function make-indexes (dir)
  ;; indexes = section-index ...
  ;; section-index = (section file-index ...)
  ;; file-index = ((page ...) one-line-desc file-name)
  (let (indexes nil)
    (dolist (file (.children dir))
      (if (man-file? file) (push! (make-index dir file) indexes)))
    (cons (man-dir->section dir)
          (sort! indexes :sorter (f (x y) (< (memcmp x y) 0)) :key caar))))

(function! main (args)
  (with-open ($out $man-indexes :write)
    (dolist (dir (.children $man-root))
      (if (man-dir? dir) (write (make-indexes dir))))))
