; markdown to html.

(import :xml)
(import :markdown)

(<- $default-css
"
/* expanded inline */
body {
  width:780px;
  margin:auto;
  font-family:Consolas, 'Courier New', Courier, Monaco, monospace;
}
ul.index { padding-left:0px; list-style:none; }
ul.index span { margin-left:1em; }
ul.index a { color:#000; text-decoration:none; margin:2px; overflow:visible; }
pre, blockquote, table { margin-left:1em; margin-right:1em; padding:0.5em; }
h1 { margin-top:0.25em; }
h1, h2, h3, h4, h5, h6, p, pre, blockquote, table { margin-bottom:0.75em; }
h1 { font-size:1.6em; }
h2 { font-size:1.2em; }
h1, h2, h3 { border-bottom:solid 1px #ccc; }
h3, h4, h5, h6 { font-size:1.0em; }
h1, h2, h3, h4, h5, h6 { display:block; margin-top:0.75em; font-weight:bold; }
p { text-indent:1em; }
pre { border:solid 1px #ccc; box-sizing:border-box; overflow-x:auto; }
blockquote { padding-left:1em; border-left:1.2px solid #ccc; }
table { border-collapse:collapse; }
thead { border-bottom:1.2px solid #ccc; }
th, td { padding:3px; }
th:nth-child(1), td:nth-child(1) { border-right:1.2px solid #ccc; }
"
    $nodes nil
    $outline nil
    $outline-level (array 6))

(function header-id ()
  (with-memory-stream (out)
    (doarray (x $outline-level)
      (if (= x 0) (break))
      (write x out :end "")
      (write-bytes "." out))))

(function hx-level (hx)
  (switch hx
    'h1 1
    'h2 2
    'h3 3
    'h4 4
    'h5 5
    'h6 6))

(function parse-header (node)
  (let (id (header-id) level (hx-level node))
    (dotimes (i 6)
      (if (= i (-- level)) ([]<- $outline-level i ([] $outline-level i))
          (> i (-- level)) ([]<- $outline-level i 0)))
    (push! $outline (<- node `(,(car node) (:id ,id) ,@(cdr node))))
    node))

(function parse-nodes (nodes)
  (map (lambda (node)
         (if (find-if (lambda (hx)
                        (eq? hx (car nodes)))
                      '(h1 h2 h3 h4 h5 h6))
             (parse-header node)
             node))
       nodes))

(function! main (args)
  (if (nil? (cadr args)) (error "require markdown file path"))
  (let (p (Path.of (cadr args)))
    (dotimes (i 6) ([]<- $outline-level i 0))
    (with-open (in p :read)
      (<- $nodes (parse-nodes (.read-all (.init (.new MarkdownReader) in)))))
    (with-open (out (Path.of (string (.but-suffix p) ".html")) :write)
      (dolist (x `((!DOCTYPE "html")
                   (html (:lang "ja")
                         (head (meta (:charset ,(bytes->string $external-encoding)))
                               (style ,$default-css))
                         (body
                           ,@$outline
                           ,@$nodes))))
        (write-bytes (xml->string x) out)))))
