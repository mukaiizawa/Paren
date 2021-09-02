; manual dependency.

(import :optparse)
(import :man (.resolve $paren-home "tools"))

(function parse-see-also (file)
  ;; Returns the list like a `(page dst ...)`.
  (with-open ($in file :read)
    (read-line)
    (let (line (read-line) (pages one-line-desc) (man-parse-one-line-desc line))
      (while (<- line (read-line))
        (if (= line "# SEE ALSO") (break)))
      (return (cons (car pages)
                    (map (f (x) (slice x 2)) (collect read-line)))))))

(function walk-dir (root)
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

(function exist? (indexes section-page)
  (let ((section page) (man-split-section-page section-page))
    (find (f (x) (in? page (car x)))
          (find (f (x) (if (= (car x) section) (cdr x)))
                indexes))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "a") args) show-all? (.get op "a")
                  dependencies (walk-dir $man-root) indexes (man-indexes))
    (dolist (dep dependencies)
      (let ((src :rest dst...) dep)
        (dolist (dst dst...)
          (let (exist? (exist? indexes dst))
            (if (|| (! exist?) show-all?) (write (list :src src :dst dst :exist? exist?)))))))))
