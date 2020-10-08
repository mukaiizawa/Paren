; markdown to html.

(import :xml)
(import :markdown)

(<- $default-css
"
body {
  width:780px;
  margin:auto;
  font-family:Consolas, 'Courier New', Courier, Monaco, monospace;
}
ul.contents { padding-left:0px; list-style:none; }
ul.contents span { margin-left:1em; }
ul.contents a { color:#000; text-decoration:none; margin:2px; overflow:visible; }
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
    $headers '(h1 h2 h3 h4 h5 h6)
    $contents nil)

(function contents-padding (id)
  (let (i -1 pad nil)
    (while (<- i (memstr id "." (++ i)))
      (push! pad '(span)))
    pad))

(function parse-contents (contentss)
  `((h1 "0. Contents")
    (ul (:class "contents")
        ,@(map (f (node)
                 (let (id (cadadr node))
                   `(li (:id ,(string 'contents- id))
                        (a (:href ,(string "#" id)) ,@(contents-padding id) ,@(cddr node)))))
               contentss))))

(function next-id (contents-index)
  (with-memory-stream ($out)
    (doarray (x contents-index)
      (if (= x 0) (break))
      (write-mem (string x ".")))))

(function parse-header (contents-index node)
  (let (hx (car node) x (- ([] hx 1) 0x30))
    (dotimes (i 6)
      (if (= i (-- x)) ([] contents-index i (++ ([] contents-index i)))
          (> i (-- x)) ([] contents-index i 0)))
    (let (id (next-id contents-index))
      (push! $contents `(,hx (:id ,id) ,@(cons id (cdr node)))))))

(function header? (node)
  (find-if (f (hx) (eq? hx (car node))) $headers))

(function parse-nodes (nodes)
  (let (contents-index (array 6))
    (dotimes (i 6) ([] contents-index i 0))
    (map (f (node)
           (if (header? node) (parse-header contents-index node)
               node))
         nodes)))

(function make-html (nodes)
  `((!DOCTYPE "html")
    (html (:lang "ja")
          (head
            (meta (:charset "UTF-8"))
            (style ,$default-css))
          (body
            ,@(parse-contents (reverse! $contents))
            ,@nodes))))

(function! main (args)
  (if (nil? (cadr args)) (error "require markdown file path"))
  (let (p (Path.of (cadr args)) nodes nil)
    (with-open ($in p :read)
      (<- nodes (parse-nodes (.reads (.new MarkdownReader)))))
    (with-open ($out (string (.but-suffix p) ".html") :write)
      (write-lines (map xml->string (make-html nodes))))))
