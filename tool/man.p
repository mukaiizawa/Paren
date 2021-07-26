; man.

(<- $search-path (map ++ (.. 7)))

(function man (page section)
  (let (root (.resolve $paren-home "man"))
    (dolist (section (if section (list section) $search-path))
      (let (file (.resolve root (str "man" section "/" page ".md")))
        (if (.readable? file) 
            (catch (Exception (f (e) nil))
              (write-line (str (.base-name file) "(" section ")"))
              (write-line)
              (with-open ($in file :read)
                (write-bytes (read-bytes)))
              (return true)))))))

(function parse-args (args)
  ;; Returns '(page section).
  (if (nil? args) (list "man" 1)
      (cdr args) (list (cadr args) (int (car args)))
      (! (suffix? (car args) ")")) (list (car args) nil)
      (let (arg1 (car args) i (- (len arg1) 3))
        (list (slice arg1 0 i) (int (slice arg1 (+ i 1) (+ i 2)))))))

(function! main (args)
  (apply man (parse-args args)))
