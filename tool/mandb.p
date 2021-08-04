; mandb.

(import :man (.resolve $paren-home "tool"))

(function make-index (file)
  ;; page [', ' page] ... ' - ' one-line-desc
  (with-open ($in file :read)
    (if (! (prefix? (read-line) "# NAME")) (raise ArgumentError "illegal manual file")
        (return (concat (man-parse-one-line-desc (read-line)) (list (.to-s file)))))))

(function make-indexes (dir)
  ;; indexes = section-index ...
  ;; section-index = (section file-index ...)
  ;; file-index = ((page ...) one-line-desc file-name)
  (let (indexes nil)
    (dolist (file (.children dir))
      (if (man-file? file) (push! (make-index file) indexes)))
    (cons (man-dir->section dir)
          (sort! indexes :sorter (f (x y) (< (memcmp x y) 0)) :key caar))))

(function! main (args)
  (with-open ($out $man-indexes :write)
    (dolist (dir (.children $man-root))
      (if (man-dir? dir) (write (make-indexes dir))))))
