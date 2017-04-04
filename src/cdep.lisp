(require :stdlib *module-stdlib*)
(require :regex *module-regex*)

(defun collect-c-files ()
  (remove-if (lambda (path)
               (string/= (pathname-type path) "c"))
             (ls "./" :dir nil)))

(defun collect-depend (path)
  (labels ((walk (path traversed)
                 (unless (file-exists? path)
                   (error "cdep: File not found ~A." path))
                 (dolist (line (read-from path))
                   (when (match?->string "^#include \".*\"" line)
                     (let* ((quoted (match?->string "\".*\"" line))
                            (dep (subseq quoted 1 (1- (length quoted)))))
                       (unless (find dep traversed :test #'string=)
                         (walk dep (push dep traversed))))))
                 traversed))
    (walk path nil)))

(defun print-depend ()
  (dolist (file.c (collect-c-files))
    (let* ((file (pathname-name file.c))
           (file_t.c? (match? "_t$" file)))
      (princln
        (list->string
          (list*
            (mkstr file (if file_t.c? "$(exe)" ".o") ":")
            (file-namestring file.c)
            (mkstr-if file_t.c?
              (subseq file 0 (- (length file) 2)) ".o")
            (collect-depend file.c))
          " "))
      (when file_t.c?
        (princln
          (mkstr #\tab "$(CC) -o " file "$(exe) " file ".c "
                 (list->string
                   (mapcar (lambda (path)
                             (mkstr (pathname-name path) ".o"))
                           (collect-depend (mkstr (match?->string "^[^_]*" file) ".c")))
                   " ")))
        (princln (mkstr #\tab "$(pref)" file "$(exe)"))))))

(defun main ()
  (write-to!
    (with-output-to-string (s)
      (let ((*standard-output* s))
        (print-depend)))
    "cdep.d"))

(main)
