; outline.

(import :optparse)
(import :re)

(function outline-matcher (option)
  (let (h (.get option "h") l (.get option "l"))
    (re.compile (str "^#{1," (if h 1 l (int l) 6) "} "))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "hl:") args)
                  matcher (outline-matcher op))
    (foreach write-line
             (select (f (x) (re.match matcher x))
                     (collect read-line)))))
