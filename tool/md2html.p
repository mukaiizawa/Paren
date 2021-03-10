; markdown to html.

(import :optparse)
(import :markdown)

(<- $usage
"
Usage: paren md2html.p [OPTION]
	Convert the markdown file read from the standard input into an html file and output it to the standard output.
OPTION:
	c -- Specify charset. If omitted, it is considered that 'UTF-8' is specified.
	C -- Do not output table of contents.
	t -- Consider the first line as the title.
"
    $default-css
"
::-webkit-scrollbar { width:8px; height:8px; }
::-webkit-scrollbar-thumb { background-color:rgba(40, 40, 40, .5); border-radius:5px; }
body { margin:0; display:flex; justify-content:center; font-family:Consolas, 'Courier New', Courier, Monaco, monospace; }
aside { width:25vw; overflow:scroll; margin-left:1em; max-height:100vh; position:sticky; top:0; font-size:0.9rem; }
aside ul { padding-left:0px; list-style:none; }
aside ul span { margin-left:1em; }
aside ul a { color:#000; text-decoration:none; margin:2px; white-space:nowrap; }
article { width:75vw; margin-left:1em; }
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
    $default-charset "UTF-8"
    $headers '(h1 h2 h3 h4 h5 h6)
    $contents nil)

(function contents-padding (id)
  (let (i -1 pad nil)
    (while (<- i (memmem id "." (++ i)))
      (push! '(span ()) pad))
    pad))

(function parse-contents (contentss)
  `((aside
      (h1 "Table of Contents")
      (ul (:class "contents")
          ,@(map (f (node)
                   (let (id (cadadr node))
                     `(li (:id ,(string 'contents- id))
                          (a (:href ,(string "#" id)) ,@(contents-padding id) ,@(cddr node)))))
                 contentss)))))

(function next-id (contents-index)
  (with-memory-stream ($out)
    (doarray (x contents-index)
      (if (= x 0) (break))
      (write-mem (string x ".")))))

(function parse-header (contents-index node)
  (let ((hx hx-attr :rest hx-text) node x (- ([] hx 1) 0x30))
    (dotimes (i 6)
      (if (= i (-- x)) ([] contents-index i (++ ([] contents-index i)))
          (> i (-- x)) ([] contents-index i 0)))
    (let (id (next-id contents-index))
      (push! `(,hx (:id ,id) ,@(cons id hx-text)) $contents))))

(function parse-nodes (nodes)
  (let (contents-index (array 6))
    (dotimes (i 6) ([] contents-index i 0))
    (map (f (node)
           (if (include? (car node) $headers) (parse-header contents-index node)
               node))
         nodes)))

(function make-html (nodes :key title? charset output-contents?)
  (let (title nil table-of-contents (if output-contents? (parse-contents (reverse! $contents))))
    (if title? (<- title `((title () ,(caddar nodes))) nodes (cdr nodes)))
    `((!DOCTYPE "html")
      (html (:lang "ja")
            (head ()
                  (meta (:charset ,(|| charset $default-charset)))
                  ,@title
                  (style () ,$default-css))
            (body ()
                  ,@table-of-contents
                  (article ,@nodes))))))

(function! main (args)
  (catch (Error (f (e) (write-line $usage) (throw e)))
    (let ((op args) (.parse (.init (.new OptionParser) "tCc:") args) rd (.new MarkdownReader))
      (foreach (f (x) (write-line (xml->str x)))
               (make-html (parse-nodes (collect (f () (.read rd))))
                          :title? (.get op "t")
                          :charset (.get op "c")
                          :output-contents? (! (.get op "C")))))))
