; xml to text.

(import :xml)

(function write-text (dom)
  (foreach (f (x) (if (string? x) (write-line x) (write-text x)))
           (cddr dom)))

(function! main (args)
  (foreach write-text
           (collect (partial .read (.new XMLReader)))))
