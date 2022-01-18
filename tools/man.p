; man.

;; man, mandb, whatis.

(<- $man-root (.resolve $paren-home "man")
    $man-indexes (.resolve $man-root "indexes.p")
    $man-sections (map ++ (.. 7)))

(function man-indexes (:opt sections)
  ; Returns a read list of manual index files.
  (assert (.file? $man-indexes))
  (with-open ($in $man-indexes :read)
    (let (indexes (collect read))
      (return (if (nil? sections) indexes
                  (select (f (x) (in? (car x) (->list sections)))
                          indexes))))))

(function man-dir? (dir)
  ; Returns whether dir is a manual directory.
  (&& (.dir? dir) (prefix? (.name dir) "man")))

(function man-dir->section (dir)
  ; Returns the section of the manual directory dir.
  (slice (.name dir) 3))

(function man-file? (file)
  ; Returns whether the file is manual.
  (&& (.file? file) (= (.suffix file) "md")))

(function man-parse-name (line)
  ; Returns the list like a `((page ...) one-line-desc)`.
  (with-memory-stream ($in line)
    (let (pages nil rd (.new AheadReader))
      (loop
        (if (= (.next rd) " ")
            (begin
              (push! (.token rd) pages)
              (dostring (ch " - ") (.skip rd))
              (while (.next rd) (.get rd))
              (return (list (reverse! pages) (.token rd))))
            (= (.next rd) ",")
            (begin
              (.skip rd ",")
              (if (= (.next rd) " ")
                  (begin
                    (.skip rd)
                    (push! (.token rd) pages))
                  (.put rd ",")))
            (.get rd))))))

(function man-split-section-page (section-page)
  ; Returns the list like a `(section man)`
  (let (i (- (len section-page) 3))
    (list (slice section-page (+ i 1) (+ i 2)) (slice section-page 0 i))))

(function man-merge-section-page (section page)
  ; Returns a string that uniquely identifies the manual.
  (str page "(" section ")"))

(function man-walk (fn indexes)
  ;; walk indexes with fn until fn returns true.
  ;; Returns whether fn returned true.
  ;; (fn section pages one-line-desc file-name)
  (dolist (index indexes)
    (let ((section :rest nodes) index)
      (dolist (node nodes)
        (if (apply fn (cons section node)) (return true))))))

;; man.

(function similar? (s t)
  (let (threefold 4 distance (f (s t d)
                               (if (= d threefold) threefold
                                   (empty? s) (+ (len t) d)
                                   (empty? t) (+ (len s) d)
                                   (= ([] s 0) ([] t 0)) (distance (slice s 1) (slice t 1) d)
                                   (min (distance s (slice t 1) (++ d))
                                        (distance (slice s 1) t (++ d))
                                        (distance (slice s 1) (slice t 1) (++ d))))))
    (< (distance s t 0) threefold)))

(function fuzzy-man (indexes section page)
  (let (similar-pages nil)
    (man-walk (f (section pages one-line-desc file-name)
                (foreach (f (x) (if (similar? page x) (push! (man-merge-section-page section x) similar-pages)))
                         pages))
              indexes)
    (write-line "Not Found.")
    (when similar-pages
      (write-line)
      (write-line "The similar pages are")
      (foreach (f (x) (write-line (format "  - %s" x)))
               (reverse! similar-pages)))))

(function man (indexes section page)
  (man-walk (f (section pages one-line-desc file-name)
              (when (in? page pages)
                (write-line (man-merge-section-page section page))
                (write-line)
                (with-open ($in (.resolve $man-root file-name) :read)
                  (write-bytes (read-bytes)))
                (write-line)
                (write-line "--")
                (write-line file-name)
                (return true)))
            indexes))

(function xint? (s)
  (catch (Error (f (e) nil))
    (int s)))

(function parse-args (args)
  ;; Returns '(section page).
  (let ((:opt arg1 arg2 :rest rest-args) args)
    (if (nil? args) (list "1" "man")
        (nil? arg2) (if (suffix? arg1 ")") (man-split-section-page arg1) (list nil arg1))
        (let (section nil page-names nil)
          (if (xint? arg1) (<- section arg1 page-names (cons arg2 rest-args))
              (<- page-names (cons arg1 (cons arg2 rest-args))))
          (list section (join page-names "-"))))))

(function! main (args)
  (catch (OSError (f (e) nil))
    (let ((section page) (parse-args args) indexes (man-indexes section))
      (if (nil? (man indexes section page)) (fuzzy-man indexes section page)))))
