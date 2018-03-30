(require :stdlib *module-stdlib*)
(require :regex *module-regex*)

(defun collect-c-files ()
  (remove-if (lambda (path)
               (string/= (pathname-type path) "c"))
             (ls "./" :dir nil)))

(defun collect-depend (path)
  (let ((traversed nil))
    (funcall (alambda (path)
               (when (file-exists? path)
                 (dolist (line (read-from path))
                   (when (match?->string "^#include \".*\"" line)
                     (let* ((quoted (match?->string "\".*\"" line))
                            (dep (subseq quoted 1 (1- (length quoted)))))
                       (unless (find dep traversed :test #'string=)
                         (push dep traversed)
                         (self dep)))))))
             path)
    traversed))

(defun print-depend ()
  (dolist (cpath (collect-c-files))
    (let ((file (pathname-name cpath)))
      (princln
        (list->string
          (list*
            (mkstr file ".o:")
            (file-namestring cpath)
            (collect-depend cpath))
          " ")))))

(defun main ()
  (with-open-file (out (make-pathname :name "cdep.d")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((*standard-output* out))
      (print-depend))))

(main)
