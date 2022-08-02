; markdown to html.

(import :optparse)
(import :markdown)

(<- $default-css
"
::-webkit-scrollbar { width:8px; height:8px; }
::-webkit-scrollbar-thumb { background-color:rgba(40, 40, 40, .5); border-radius:5px; }
body { margin:0; display:flex; justify-content:center; font-family:Consolas, 'Courier New', Courier, Monaco, monospace; }
aside { width:25vw; overflow:scroll; margin-left:1rem; max-height:100vh; position:sticky; top:0; font-size:0.9rem; }
aside ul { padding-left:0px; list-style:none; }
aside ul span { margin-left:1rem; }
aside ul a { color:#000; text-decoration:none; margin:2px; white-space:nowrap; }
article { width:75vw; margin-left:1rem; }
h1 { margin-top:0.25rem; }
h1, h2, h3, h4, h5, h6, p, pre, blockquote, table { margin-bottom:0.75rem; }
h1 { font-size:1.6rem; }
h2 { font-size:1.2rem; }
h1, h2, h3 { border-bottom:solid 1px #ccc; }
h3, h4, h5, h6 { font-size:1.0rem; }
h1, h2, h3, h4, h5, h6 { display:block; margin-top:0.75rem; font-weight:bold; }
th, td { padding:0.2em 0.5rem; }
em { font-weight:bold; font-style:normal; }
p { text-indent:1rem; }
ul p, ol p { text-indent:0; margin:0; }
pre, blockquote, table { margin-left:1rem; margin-right:1rem; }
pre { padding:1rem; overflow-x:auto; border:1px solid #ddd; }
code { font-size:1rem; font-family:monospace; }
blockquote { padding-left:1rem; border-left:1.2px solid #ccc; }
table { border-collapse:collapse; }
thead { border-bottom:1.2px solid #ccc; }
th:nth-child(1), td:nth-child(1) { border-right:1.2px solid #ccc; }
"
    $headers '(h1 h2 h3 h4 h5 h6)
    $contents nil)

(function contents-padding (id)
  (let (padding nil)
    (dotimes (i (count (partial = ".") (list... id)))
      (push! '(span ()) padding))
    padding))

(function parse-contents (contents)
  `((aside ()
           (h1 () "Table of Contents")
           (ul (:class "contents")
               ,@(map (f (node)
                        (let (id (cadadr node))
                          `(li (:id ,(str 'contents- id))
                               (a (:href ,(str "#" id)) ,@(contents-padding id) ,@(cddr node)))))
                      contents)))))

(function next-id (contents-index)
  (with-memory-stream ($out)
    (doarray (x contents-index)
      (if (= x 0) (break))
      (write-bytes (str x ".")))))

(function parse-header (contents-index node)
  (let ((hx hx-attr :rest hx-text) node x (int ([] (str hx) 1)))
    (dotimes (i 6)
      (if (= i (-- x)) ([] contents-index i (++ ([] contents-index i)))
          (> i (-- x)) ([] contents-index i 0)))
    (let (id (next-id contents-index))
      (push! `(,hx (:id ,id) ,@(cons id hx-text)) $contents))))

(function parse-nodes (nodes)
  (let (contents-index (array 6))
    (dotimes (i 6) ([] contents-index i 0))
    (map (f (node)
           (if (in? (car node) $headers) (parse-header contents-index node)
               node))
         nodes)))

(function make-html (nodes :key output-table-of-contents?)
  (let (title? (= (caar nodes) 'p)
               (title nodes) (if title? (list `((title () ,(caddar nodes))) (cdr nodes)) (list nil nodes))
               table-of-contents (if output-table-of-contents? (parse-contents (reverse! $contents))))
    `(html (:lang "ja")
           (head ()
                 (meta (:charset "UTF-8"))
                 ,@title
                 (style () ,$default-css))
           (body ()
                 ,@table-of-contents
                 (article ()
                          ,@nodes)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "c") args) rd (.new MarkdownReader))
    (dom.write (make-html (parse-nodes (collect (f () (.read rd))))
                          :output-table-of-contents? (! (.get op "c"))))))
