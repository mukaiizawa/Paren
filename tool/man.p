; man.

;; man, mandb, whatis.

(<- $man-root (.resolve $paren-home "man")
    $man-indexes (.resolve $man-root "indexes.p")
    $man-sections (map ++ (.. 7)))

(function man-indexes ()
  ; Returns a read list of manual index files.
  (with-open ($in $man-indexes :read)
    (return (collect read))))

(function man-dir? (dir)
  ; Returns whether dir is a manual directory.
  (&& (.dir? dir) (prefix? (.name dir) "man")))

(function man-dir->section (dir)
  ; Returns the section of the manual directory dir.
  (slice (.name dir) 3))

(function man-file? (file)
  ; Returns whether the file is manual.
  (&& (.file? file) (= (.suffix file) "md")))

(function man-parse-one-line-desc (line)
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

;; man.

(function man (section page)
  (dolist (indexes (man-indexes))
    (if (&& section (!= section (car indexes))) (continue)
        (dolist (index (cdr indexes))
          (let ((pages one-line-desc file-name) index)
            (if (in? page pages)
                (let (section (car indexes))
                  (write-line (man-merge-section-page section page))
                  (write-line)
                  (with-open ($in (.resolve $man-root file-name) :read)
                    (write-bytes (read-bytes))))))))))

(function parse-args (args)
  ;; Returns '(section page).
  (if (nil? args) (list "1" "man")
      (cdr args) args
      (suffix? (car args) ")") (man-split-section-page (car args))
      (list nil (car args))))

(function! main (args)
  (catch (OSError (f (e) nil))
    (if (.file? $man-indexes) (apply man (parse-args args))
        (raise StateError "missing indexes file. First run the program `paren mandb`"))))
