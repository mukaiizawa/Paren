; man.

(<- $search-path (map ++ (.. 7)))

(function display (section page file)
  (catch (Error (f (e) (.print-stack-trace e) nil))
    (with-open ($in file :read)
      (let (line (read-line))
        (if (prefix? line "# NAME")
            (begin
              (write-line (str page "(" section ")"))
              (write-line)
              (write-line line)
              (write-bytes (read-bytes)))
            (prefix? line "[")
            (let (url (slice line (++ (strstr line "(")) (-- (len line))))
              (display section page (.resolve (.parent file) url))))))))

(function man (section page)
  (let (root (.resolve $paren-home "man"))
    (dolist (section (if section (list section) $search-path))
      (let (file (.resolve root (str "man" section "/" page ".md")))
        (when (.file? file)
          (display section page file)
          (return true))))))

(function parse-args (args)
  ;; Returns '(section page).
  (if (nil? args) (list 1 "man")
      (cdr args) (list (int (car args)) (cadr args))
      (! (suffix? (car args) ")")) (list nil (car args))
      (let (arg1 (car args) i (- (len arg1) 3))
        (list (int (slice arg1 (+ i 1) (+ i 2))) (slice arg1 0 i)))))

(function! main (args)
  (apply man (parse-args args)))
