; man.

;; man, mandb, whatis.

(<- $man-root (.resolve $paren-home "man")
    $man-indexes (.resolve $man-root "indexes.wk")
    $man-sections (map ++ (.. 7)))

(function man-dir? (dir)
  (&& (.dir? dir) (prefix? (.name dir) "man")))

(function man-dir->section (dir)
  (slice (.name dir) 3))

(function man-file? (file)
  (&& (.file? file) (= (.suffix file) "md")))

(function man-header? (line)
  (prefix? line "# NAME"))

(function man-indexes ()
  (with-open ($in $man-indexes :read)
    (return (collect read))))

;; man.

(function man (section page)
  (dolist (indexes (man-indexes))
    (if (&& section (!= section (car indexes))) (continue)
        (dolist (index (cdr indexes))
          (let ((pages one-line-desc file-name) index)
            (if (in? page pages)
                (let (section (car indexes))
                  (write-line (str page "(" section ")"))
                  (write-line)
                  (with-open ($in file-name :read)
                    (write-bytes (read-bytes))))))))))

(function parse-args (args)
  ;; Returns '(section page).
  (if (nil? args) (list nil"man")
      (cdr args) (list (car args) (cadr args))
      (! (suffix? (car args) ")")) (list nil (car args))
      (let (arg1 (car args) i (- (len arg1) 3))
        (list (slice arg1 (+ i 1) (+ i 2)) (slice arg1 0 i)))))

(function! main (args)
  (catch (OSError (f (e) nil))
    (if (.file? $man-indexes) (apply man (parse-args args))
        (raise StateError "missing indexes file. First run the program `paren mandb`"))))
