(require :stdlib *module-stdlib*)
(require :regex *module-regex*)

(defun collect-c-files ()
  (remove-if (lambda (path)
               (string/= (pathname-type path) "c"))
             (ls "./" :dir nil)))

(defun collect-depend ()
  (labels ((walk (path traversed)
                 (dolist (line (read-from path))
                   (when (match?->string "^#include \".*\"" line)
                     (let* ((quoted (match?->string "\".*\"" line))
                            (dep (subseq quoted 1 (1- (length quoted)))))
                       (unless (find dep traversed :test #'string=)
                         (walk dep (push dep traversed))))))
                 traversed))
    (dolist (cfile (collect-c-files))
      (let ((test-file? (match? "_t$" (pathname-name cfile)))
            (base-name (pathname-name cfile)))
        (princln
          (list->string
            (list*
              (mkstr base-name (unless test-file? ".o") ":")
              (file-namestring cfile)
              (walk cfile nil))
            " "))
        (when test-file?
          (princln
            (mkstr #\tab "$(CC) -o " base-name "$(exe) "
                   base-name ".c "
                   (list->string
                     (mapcar (lambda (path)
                               (mkstr (pathname-name path) ".o"))
                             (walk (mkstr
                                     (match?->string "^[^_]*" base-name) ".c")
                                   nil))
                     " ")))
          (princln (mkstr #\tab "$(pref)" base-name "$(exe)")))))))

(defun main ()
  (write-to!
    (with-output-to-string (s)
      (let ((*standard-output* s))
        (collect-depend)))
    "cdep.d"))

(main)
