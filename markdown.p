; markdown module.

(import :xml)

(class MarkdownReader (AheadReader))

(method MarkdownReader .stmt? ()
  (&& (.next self) (string/= (.next self) "\n")))

(method MarkdownReader .parse-tr (:opt tx)
  (let (txlist nil)
    (.skip self "|")
    (while (string/= (.next self) "\n")
      (while (string/= (.next self) "|")
        (.get self))
      (.skip self)
      (push! txlist (list tx (.token self))))
    (.skip self)
    `(tr () ,@(reverse! txlist))))

(method MarkdownReader .parse-table ()
  (let (thlist (.parse-tr self 'th) sep (.parse-tr self) tdlist nil)
    (while (.stmt? self)
      (push! tdlist (.parse-tr self 'td)))
    `(table
       (thead () ,thlist)
       (tbody () ,@(reverse! tdlist)))))

(method MarkdownReader .parse-list ()
  (let (next-root nil next-depth nil node-stack nil
                  fetch (lambda ()
                          (when (.stmt? self)
                            (<- next-depth 1)
                            (while (string= (.next self) " ")
                              (dotimes (i 4) (.skip self " "))
                              (<- next-depth (++ next-depth)))
                            (if (string= (.next self) "-")
                                (begin
                                  (.skip self)    ; - xxx
                                  (push! next-root 'ul))
                                (string= (.next self) "1")
                                (begin
                                  (.skip self) (.skip self ".")    ; 1. xxx
                                  (push! next-root 'ol))
                                (.raise self "missing list"))
                            (.skip-space self)
                            (push! node-stack (list 'li (.skip-line (.skip-space self))))))
                  rec (lambda (root depth nodes)
                        (while (fetch)
                          (if (< next-depth depth) (break)
                              (= next-depth depth) (if (neq? root (pop! next-root)) (.raise self "mixed list type")
                                                       (push! nodes (pop! node-stack)))
                              (begin
                                (push! nodes (rec (pop! next-root) next-depth (list (pop! node-stack))))
                                (when node-stack
                                  (if (neq? (pop! next-root) root) (.raise self "mixed list type")
                                      (push! nodes (pop! node-stack)))))))
                        (cons root (reverse! nodes))))
    (rec (if (string= (.next self) "-") 'ul 'ol) 1 nil)))

(method MarkdownReader .parse-quote ()
  (let (next-depth nil node-stack nil
                   fetch (lambda ()
                           (when (.stmt? self)
                             (<- next-depth 0)
                             (while (string= (.next self) ">")
                               (.skip self)
                               (<- next-depth (++ next-depth)))
                             (if (= next-depth 0) (.raise self "missing >"))
                             (.skip-space self)
                             (push! node-stack (.skip-line (.skip-space self)))))
                   rec (lambda (depth nodes)
                         (while (fetch)
                           (if (< next-depth depth) (break)
                               (= next-depth depth) (push! nodes (pop! node-stack))
                               (begin
                                 (push! nodes (rec next-depth (list (pop! node-stack))))
                                 (if node-stack (push! nodes (pop! node-stack))))))
                         (cons 'blockquote (reverse! nodes))))
    (rec 1 nil)))

(method MarkdownReader .parse-pre ()
  (let (lines nil get-line (lambda ()
                             (dotimes (i 4)
                               (.skip self " "))
                             (.skip-line self)))
    (push! lines (get-line))
    (when (.stmt? self)
      (push! lines (get-line)))
    (list 'pre (list->string (reverse! lines) "\n"))))

(method MarkdownReader .parse-header ()
  (let (level 0)
    (while (string= (.next self) "#")
      (.skip self)
      (<- level (++ level)))
    (.skip-space self)
    (if (<= 1 level  6) (list (bytes->symbol (string 'h level)) (.skip-line self))
        (.raise self "illegal header level " level))))

(method MarkdownReader .read ()
  ; Read markdown.
  ; Returns the XML.Node object corresponding to the read markdown tree.
  ; Readable markdown is follows.
  ;     <markdown> ::= <stmt> [<eol> <stmt> ...]
  ;     <stmt> ::= <header>
  ;              | <paragraph>
  ;              | <pre>
  ;              | <quote>
  ;              | <ul>
  ;              | <ol>
  ;              | <table>
  ;              | <footnote_reference>
  ;              | <xml>
  ;     <header> ::= { # | ## | ### | #### | ##### | ###### } <char> ... <eol>
  ;     <paragraph> ::= <string> <eol>
  ;     <pre> ::= '    ' <char> ... <eol> [<pre> ...]
  ;     <quote> ::= '>' ... <char> ... <eol> [<quote> ...]
  ;     <ul> ::= '-'  ... <char> ... <eol> [<ul> ...]
  ;     <ol> ::= '1.'  ... <char> ... <eol> [<ol> ...]
  ;     <table> ::= <tr> [<tr> ...]
  ;     <tr> ::= '|' <char> ... ['|' <string>] ... '|' <eol>
  ;     <string> ::= {
  ;             <code>
  ;             | <em>
  ;             | <link>
  ;             | <footnote_referrer>
  ;             | <char>
  ;         } ...
  ;     <code> ::= '`' <char> ... '`'
  ;     <em> ::= '*' <char> ... '*'
  ;     <link> ::= '[' <char> ... '](' <char> ... ')'
  ;     <footnote_referrer> ::= '[^' <char> ... ']'
  ;     <footnote_reference> ::= '[^' <char> ... ']:' <char> ...
  ;     <xml> -- a xml.
  ;     <eol> -- end on line.
  ;     <char> -- characters that have no special meaning.
  ; <stmt> other than <header> are considered to be the same <stmt> up to the blank line.
  ; If the same <stmt> line starts with 4 spaces, it is considered as a nested expression.
  (while (string= (.next self) "\n")
    (.skip self))
  (let (next (.next self))
    (if (nil? next) nil
        (string= next "#") (.parse-header self)
        (string= next " ") (.parse-pre self)
        (string= next ">") (.parse-quote self)
        (|| (string= next "-")
            (string= next "1")) (.parse-list self)
        (string= next "|") (.parse-table self)
        (string= next "[") (.parse-footnote-reference self)
        (string= next "<") (.parse-xml self)
        (.parse-paragraph self))))

(function! main (args)
  (let ($external-encoding :UTF-8)
    (with-open (in "readme.wk" :read)
      (let (node nil rd (.init (.new MarkdownReader) in))
        (while (<- node (.read rd))
          (write node))))))
