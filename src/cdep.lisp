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
                       (princ (mkstr " " dep))
                       (unless (find dep traversed :test #'string=)
                         (walk dep (cons dep traversed))))))))
    (dolist (cfile (collect-c-files))
      (let ((test-file? (match? "_t$" (pathname-name cfile))))
        (princ (pathname-name cfile))
        (princ (mkstr (unless test-file? ".o") ": "))
        (princ (file-namestring cfile))
        (walk cfile nil)
        (fresh-line)
        (when test-file?
          (princ (mkstr #\tab "$(CC) -o " (pathname-name cfile) "$(exe) "
                        (pathname-name cfile) ".c "))
          (walk (mkstr (match?->string "^[^_]*" (pathname-name cfile)) ".c") nil)
          (fresh-line)
          (princln (mkstr #\tab (pathname-name cfile) "$(exe)")))))))

(defun main ()
  (write-to!
    (with-output-to-string (s)
      (let ((*standard-output* s))
        (collect-depend)))
    "cdep.d"))

(main)

; これを生成したい
; clang -o queue_t.exe queue_t.c std.o queue.o
