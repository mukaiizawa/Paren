; xml to text.

(import :dom)
(import :xml)

(function! main (args)
  (write-line (dom.text-content (xml.read))))
