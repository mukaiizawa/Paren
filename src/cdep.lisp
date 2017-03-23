(require :stdlib *module-stdlib*)
(require :regex *module-regex*)

(defvar depend nil)

(defun collect-c-files ()
  (remove-if (lambda (path)
               (string/= (pathname-type path) "c"))
             (ls "./" :dir nil)))

(defun collect-depend ()
  (labels ((rec (path)
                (dolist (line (read-from path))
                  (when (match? "^#include \".*\"" line)
                    (let ((match (match?->string "\".*\"" line)))
                      (princ (mkstr " " match)))))))
    (dolist (cfile (collect-c-files))
      (princ (mkstr (pathname-name cfile) ".o: " (file-namestring cfile)))
      (rec cfile)
      (fresh-line))))

(collect-depend)
