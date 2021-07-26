; man.

(<- $search-path (map ++ (.. 7)))

(function man (section page)
  (let (root (.resolve $paren-home "man"))
    (dolist (section (if section (list section) $search-path))
      (let (file (.resolve root (str "man" section "/" page ".md")))
        (if (.readable? file) 
            (catch (Exception (f (e) nil))
              (with-open ($in file :read)
                (write-bytes (read-bytes)))
              (return true)))))))

(function! main (args)
  (if (nil? args) (man 1 "man")
      (nil? (cdr args)) (man nil (car args))
      (man (int (car args)) (cadr args))))
