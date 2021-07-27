; whatis.

(import :optparse)

(function display (section page file)
  (catch (Error (f (e) nil))
    (with-open ($in file :read)
      (let (line (read-line))
        (if (prefix? line "# NAME")
            (write-line
              (format "%-20s %s" (str page "(" section ")") (slice (read-line) (len (.base-name file)))))
            (prefix? line "[")
            (let (url (slice line (++ (strstr line "(")) (-- (len line))))
              (display section page (.resolve (.parent file) url))))))))

(function whatis (matcher)
  (dolist (dir (.children (.resolve $paren-home "man")))
    (if (.dir? dir)
        (let (section (last (.name dir)))
          (dolist (file (.children dir))
            (let (page (.base-name file))
              (if (matcher section page) (display section page file))))))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "s:") args)
                  page (car args) sections (split (.get op "s") ","))
    (whatis (f (s p)
              (&& (|| (nil? sections) (in? s sections))
                  (|| (nil? page) (in? page p)))))))
