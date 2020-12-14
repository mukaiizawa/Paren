; markdown module.

(import :xml)

(class MarkdownReader (XMLReader))

(method MarkdownReader .none-match? (:rest args)
  (let (next (.next self))
    (&& next (none? (f (x) (memeq? next x)) (cons "\n" args)))))

(method MarkdownReader .continue? ()
  (.none-match? self))

(method MarkdownReader .parse-header ()
  (let (level 0)
    (while (memeq? (.next self) "#")
      (.skip self)
      (<- level (++ level)))
    (if (<= 1 level  6) (list (mem->sym (string 'h level)) () (.skip-line (.skip-space self)))
        (.raise self "illegal header level " level))))

(method MarkdownReader .parse-code ()
  (.skip self)
  (while (.none-match? self "`") (.get self))
  (.skip self "`")
  `(code ,(.token self)))

(method MarkdownReader .parse-em ()
  (.skip self)
  (while (.none-match? self "*") (.get self))
  (.skip self "*")
  `(em ,(.token self)))

(method MarkdownReader .parse-footnote-referrer ()
  (.skip self)    ; ^
  (while (.none-match? self "]") (.get self))
  (.skip self "]")
  (let (i (.token self))
    `(sup (:id ,(string "fnrefere" i))
          (a (:href ,(string "#fnreferr" i)) ,i))))

(method MarkdownReader .parse-link ()
  (while (.none-match? self "]") (.get self))
  (.skip self "]")
  (let (text (.token self))
    (.skip self "(")
    (while (.none-match? self ")") (.get self))
    (.skip self ")")
    `(a (:href ,(.token self)) ,text)))

(method MarkdownReader .parse-ref ()
  (.skip self)
  (if (memeq? (.next self) "^") (.parse-footnote-referrer self)
      (.parse-link self)))

(method MarkdownReader .parse-paragraph ()
  (let (children nil text nil)
    (while (.continue? self)
      (if (memeq? (.next self) "`") (push! children (.parse-code self))
          (memeq? (.next self) "*") (push! children (.parse-em self))
          (memeq? (.next self) "[") (push! children (.parse-ref self))
          (begin
            (while (.none-match? self "`" "*" "[") (.get self))
            (if (memneq? (<- text (.token self)) "") (push! children text)))))
    `(p () ,@(reverse! children))))

(method MarkdownReader .parse-pre ()
  (let (get-line (f ()
                   (when (memeq? (.next self) " ")
                     (dotimes (i 4) (.skip self " "))
                     (.skip-line self))))
    `(pre () ,(join (collect get-line) "\n"))))

(method MarkdownReader .parse-quote ()
  (let (next-depth nil node-stack nil
                   fetch (f ()
                           (when (.continue? self)
                             (<- next-depth 0)
                             (while (memeq? (.next self) ">")
                               (.skip self)
                               (<- next-depth (++ next-depth)))
                             (if (= next-depth 0) (.raise self "missing >"))
                             (.skip-space self)
                             (push! node-stack (.skip-line (.skip-space self)))))
                   rec (f (depth nodes)
                         (while (fetch)
                           (if (< next-depth depth) (break)
                               (= next-depth depth) (begin
                                                      (if (&& nodes (string? (car nodes))) (push! nodes '(br)))
                                                      (push! nodes (pop! node-stack)))
                               (begin
                                 (push! nodes (rec next-depth (list (pop! node-stack))))
                                 (if node-stack (push! nodes (pop! node-stack))))))
                         `(blockquote () ,@(reverse! nodes))))
    (rec 1 nil)))

(method MarkdownReader .parse-list ()
  (let (next-root nil next-depth nil node-stack nil
                  fetch (f ()
                          (when (|| (.digit? self) (memmem "- " (.next self)))
                            (<- next-depth 1)
                            (while (memeq? (.next self) " ")
                              (dotimes (i 4) (.skip self " "))
                              (<- next-depth (++ next-depth)))
                            (if (memeq? (.next self) "-")
                                (begin
                                  (.skip self)    ; - xxx
                                  (push! next-root 'ul))
                                (.digit? self)
                                (begin
                                  (.skip-uint self) (.skip self ".")    ; 1. xxx
                                  (push! next-root 'ol))
                                (.raise self "missing list"))
                            (.skip-space self)
                            (push! node-stack (list 'li () (.skip-line (.skip-space self))))))
                  rec (f (root depth nodes)
                        (while (fetch)
                          (if (< next-depth depth) (break)
                              (= next-depth depth) (if (neq? root (pop! next-root)) (.raise self "mixed list type")
                                                       (push! nodes (pop! node-stack)))
                              (begin
                                (push! nodes (rec (pop! next-root) next-depth (list (pop! node-stack))))
                                (when node-stack
                                  (if (neq? (pop! next-root) root) (.raise self "mixed list type")
                                      (push! nodes (pop! node-stack)))))))
                        (cons root (cons nil (reverse! nodes)))))
    (rec (if (memeq? (.next self) "-") 'ul 'ol) 1 nil)))

(method MarkdownReader .parse-tr (:opt tx)
  (let (txlist nil)
    (.skip self "|")
    (while (memneq? (.next self) "\n")
      (while (memneq? (.next self) "|") (.get self))
      (.skip self "|")
      (push! txlist (list tx () (.token self))))
    (.skip self)
    `(tr () ,@(reverse! txlist))))

(method MarkdownReader .parse-table ()
  (let (thlist (.parse-tr self 'th) tdlist nil)
    (.skip-line self)    ; skip separator.
    (while (memeq? (.next self) "|")
      (push! tdlist (.parse-tr self 'td)))
    `(table ()
            (thead () ,thlist)
            (tbody () ,@(reverse! tdlist)))))

(method MarkdownReader .parse-footnote-reference ()
  (.skip self)
  (.skip self "^")
  (while (memneq? (.next self) "]") (.get self))
  (.skip self)
  (.skip self ":")
  (let (i (.token self))
    `(small (:id ,(string "fnreferr" i))
            (a (:href ,(string "#fnrefere" i)) ,(string "[" i "]"))
            ,(.skip-line (.skip-space self)))))

(method MarkdownReader .read ()
  ; Read markdown.
  ; Returns a list representation of read markdown.
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
  ;     <tr> ::= '|' <char> ... ['|' <char> ...] ... '|' <eol>
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
  (while (memeq? (.next self) "\n") (.skip self))
  (let (next (.next self))
    (if (nil? next) nil
        (memeq? next "#") (.parse-header self)
        (memeq? next " ") (.parse-pre self)
        (memeq? next ">") (.parse-quote self)
        (memeq? next "-") (.parse-list self)
        (memeq? next "1") (.parse-list self)
        (memeq? next "|") (.parse-table self)
        (memeq? next "[") (.parse-footnote-reference self)
        (memeq? next "<") (XMLReader.read self)
        (.parse-paragraph self))))

(function! main (args)
  (with-memory-stream ($in "# header1\n")
    (let (rd (.new MarkdownReader) (h1 attr val) (.read rd))
      (assert (eq? h1 'h1))
      (assert (memeq? val "header1"))))
  (with-memory-stream ($in "paragraph[^1]\n")
    (let (rd (.new MarkdownReader) (p p-attr p-val (sup (id id-val) (a (href href-val) a-val))) (.read rd))
      (assert (eq? p 'p))
      (assert (memeq? p-val "paragraph"))
      (assert (eq? sup 'sup))
      (assert (eq? id :id))
      (assert (memeq? id-val  "fnrefere1"))
      (assert (eq? a 'a))
      (assert (eq? href :href))
      (assert (memeq? href-val "#fnreferr1"))
      (assert (memeq? a-val "1"))))
  (with-memory-stream ($in "link to google [google](https://google.com)\n")
    (let (rd (.new MarkdownReader) (p p-attr p-val (a (href href-val) a-val)) (.read rd))
      (assert (eq? p 'p))
      (assert (memeq? p-val "link to google "))
      (assert (eq? a 'a))
      (assert (eq? href :href))
      (assert (memeq? href-val "https://google.com"))
      (assert (memeq? a-val "google"))))
  (with-memory-stream ($in (join '("|x|y|z|\n"
                                   "||||\n"
                                   "|a|b|c|\n")))
    (let (rd (.new MarkdownReader) (table table-attr
                                          (thead thead-attr
                                                 (th-tr th-tr-attr
                                                        (th0 th0-attr th0-val)
                                                        (th1 th1-attr th1-val)
                                                        (th2 th2-attr th2-val)))
                                          (tbody tbody-attr
                                                 (td-tr td-tr-attr
                                                        (td0 td0-attr td0-val)
                                                        (td1 td1-attr td1-val)
                                                        (td2 td2-attr td2-val)))) (.read rd))
      (assert (eq? table 'table))
      (assert (eq? thead 'thead))
      (assert (eq? tbody 'tbody))
      (assert (eq? th-tr td-tr 'tr))
      (assert (eq? th0 th1 th2 'th))
      (assert (eq? td0 td1 td2 'td))
      (assert (memeq? th0-val "x"))
      (assert (memeq? th1-val "y"))
      (assert (memeq? th2-val "z"))
      (assert (memeq? td0-val "a"))
      (assert (memeq? td1-val "b"))
      (assert (memeq? td2-val "c"))))
  (with-memory-stream ($in (join '("    foo\n"
                                   "    bar\n")))
    (let (rd (.new MarkdownReader) (pre pre-attr pre-val) (.read rd))
      (assert (eq? pre 'pre))
      (assert (memeq? pre-val "foo\nbar"))))
  (with-memory-stream ($in (join '("> bq1\n"
                                   ">> bq2\n"
                                   "> bq3\n")))
    (let (rd (.new MarkdownReader) (bq1 bq1-attr
                                        bq1-val1
                                        (bq2 bq2-attr bq2-val)
                                        bq1-val2) (.read rd))
      (assert (eq? bq1 bq2 'blockquote))
      (assert (memeq? bq1-val1 "bq1"))
      (assert (memeq? bq2-val "bq2"))
      (assert (memeq? bq1-val2 "bq3"))))
  (with-memory-stream ($in (join '("- ul1\n"
                                   "    1. ol1\n"
                                   "    1. ol2\n"
                                   "- ul2\n")))
    (let (rd (.new MarkdownReader) (ul ul-attr
                                       (ul-li1 ul-li1-attr ul-li1-val)
                                       (ol ol-attr
                                           (ol-li1 ol-li1-attr ol-li1-val)
                                           (ol-li2 ol-li2-attr ol-li2-val))
                                       (ul-li2 ul-li2-attr ul-li2-val)) (.read rd))
      (assert (eq? ul 'ul))
      (assert (eq? ol 'ol))
      (assert (eq? ul-li1 ul-li2 ol-li1 ol-li2 'li))
      (assert (memeq? ul-li1-val "ul1"))
      (assert (memeq? ul-li2-val "ul2"))
      (assert (memeq? ol-li1-val "ol1"))
      (assert (memeq? ol-li2-val "ol2"))))
  (with-memory-stream ($in (join '("<span style='color:red'>foo</span>\n")))
    (let (rd (.new MarkdownReader) (span (style style-val) span-val) (.read rd))
      (assert (eq? span 'span))
      (assert (eq? style :style))
      (assert (memeq? style-val "color:red"))
      (assert (memeq? span-val "foo"))))
  (with-memory-stream ($in (join '("[^1]: reference\n")))
    (let (rd (.new MarkdownReader) (small (id id-val) (a (href href-val) a-val) small-val) (.read rd))
      (assert (eq? small 'small))
      (assert (eq? id :id))
      (assert (memeq? id-val "fnreferr1"))
      (assert (eq? a 'a))
      (assert (eq? href :href))
      (assert (memeq? href-val "#fnrefere1"))
      (assert (memeq? a-val "[1]"))
      (assert (memeq? small-val "reference")))))
