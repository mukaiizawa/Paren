; manual dependency.

(import :optparse)
(import :re)

(import :man (.resolve $paren-home "tools"))

(function parse-see-also (file)
  ;; Returns the list like a `(page dst ...)`.
  (with-open ($in file :read)
    (read-line)
    (let (line (read-line) (pages one-line-desc) (man-parse-name line))
      (while (<- line (read-line))
        (if (= line "# SEE ALSO") (break)))
      (cons (car pages)
            (map (f (x) (slice x (++ (index "`" x)) (last-index "`" x)))
                 (collect read-line))))))

(function collect-dependencies (root)
  ;; Returns the list like a `(src dst ...) ...`.
  (let (dependencies nil)
    (dolist (dir (.children root))
      (if (man-dir? dir)
          (dolist (file (.children dir))
            (if (man-file? file)
                (let ((src-page :rest dst-section-pages) (parse-see-also file))
                  (push! (cons (man-merge-section-page (man-dir->section dir) src-page)
                               dst-section-pages)
                         dependencies))))))
    dependencies))

(function found? (indexes section-page)
  (let ((section page) (man-split-section-page section-page))
    (some? (f (x) (in? page (car x)))
           (keep1 (f (x) (if (= (car x) section) (cdr x)))
                  indexes))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "a") args)
                  show-all? (.get op "a") indexes (man-indexes))
    (dolist (dependency (collect-dependencies $man-root))
      (let ((src :rest dst...) dependency)
        (dolist (dst dst...)
          (let (found? (found? indexes dst))
            (if show-all? (write (list :src src :dst dst :broken-link? found?))
                (! found?) (write (list :src src :dst dst)))))))))
