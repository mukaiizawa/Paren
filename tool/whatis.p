; whatis.

(import :optparse)
(import :man (.resolve $paren-home "tool"))

(function whatis (matcher)
  (dolist (indexes (man-indexes))
    (let (section (car indexes))
      (dolist (index (cdr indexes))
        (let ((pages one-line-desc hash-value) index)
          (if (matcher section pages)
              (write-line
                (format "%-20s - %s" (str (join pages ", ") "(" section ")") one-line-desc))))))))

(function! main (args)
  (catch (OSError (f (e) nil))
    (let ((op args) (.parse (.init (.new OptionParser) "s:") args)
                    page (car args) sections (split (.get op "s") ","))
      (whatis (f (section pages)
                (&& (|| (nil? sections) (in? section sections))
                    (|| (nil? page) (some? (f (x) (memmem x page)) pages))))))))
