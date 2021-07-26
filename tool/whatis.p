; whatis.

(function display (dir file)
  (catch (Error (f (e) nil))
    (with-open ($in file :read)
      (read-line)    ; skip # NAME
      (let (dir-name (.name dir) name (.base-name file))
        (write-line (format "%-20s %s"
                            (str name "(" (slice dir-name (-- (len dir-name))) ")")
                            (slice (read-line) (len name))))))))

(function whatis (matcher)
  (dolist (dir (.children (.resolve $paren-home "man")))
    (if (.dir? dir)
        (dolist (file (.children dir)) 
          (if (matcher (.base-name file)) (display dir file))))))

(function! main (args)
  (if (nil? args) (whatis (f (x) true))
      (whatis (f (x) (in? (car args) x)))))
