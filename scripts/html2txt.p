; html to text.

(import :xml)

(<- $write? nil $script? nil)

(function! main (args)
  (.parse (.init (.new XMLSAXParser)
                 :on-start-element (f (name attrs)
                                     (if (== name 'body) (<- $write? true)
                                         (== name 'script) (push! true $script?)))
                 :on-end-element (f (name)
                                   (if (== name 'body) (<- $write? nil)
                                       (== name 'script) (pop! $script?)
                                       (== name 'p) (write-line)))
                 :on-read-text (f (text)
                                 (if (&& $write? (! $script?)) (write-bytes text)))
                 :on-error (f (parser error)
                             (while (! (in? (.next parser) '("<" ">")))
                               (.skip parser))
                             (.parse parser)))))
