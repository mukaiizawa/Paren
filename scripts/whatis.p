; whatis.

(import :optparse)

(import :man (.resolve $paren-home "scripts"))

(function lhs (pages :opt ommit)
  (let (val (join (concat pages (->list ommit)) ", "))
    (if (&& (cdr pages) (> (len val) 20)) (lhs (butlast pages) "...")
        val)))

(function whatis (matcher)
  (dolist (indexes (man-indexes))
    (let (section (car indexes))
      (dolist (index (cdr indexes))
        (let ((pages one-line-desc hash-value) index)
          (if (matcher section pages)
              (printf "%-20s - %s\n"
                      (lhs (map (f (x) (str x "(" section ")")) pages))
                      one-line-desc)))))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "s:") args)
                  page (car args) sections (split (.get op "s") ","))
    (whatis (f (section pages)
              (&& (|| (nil? sections) (in? section sections))
                  (|| (nil? page) (some? (partial in? page) pages)))))))
