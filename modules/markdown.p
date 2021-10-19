; markdown module.

(import :xml)

(class MarkdownReader (AheadReader XMLReader))

(method MarkdownReader .EOL? ()
  (|| (nil? (.next self)) (= (.next self) "\n")))

(method MarkdownReader .parse-header ()
  (let (level 0)
    (while (= (.next self) "#")
      (.skip self)
      (<- level (++ level)))
    (if (<= 1 level  6) (list (symbol (str 'h level)) () (.skip-line (.skip-space self)))
        (raise SyntaxError (str "illegal header level " level)))))

(method MarkdownReader .parse-quoted ()
  (let (ch (.skip self))
    (while (!= (.next self) ch) (.get self))
    (.skip self ch)
    (.token self)))

(method MarkdownReader .parse-code ()
  `(code () ,(.parse-quoted self)))

(method MarkdownReader .parse-em ()
  `(em () ,(.parse-quoted self)))

(method MarkdownReader .parse-link0 (text)
  `(a (:id ,(str "fnreferr" text) :href ,(str "#fnrefere" text))
      ,(str "[" text "]")))

(method MarkdownReader .parse-link1 (text)
  `(sup (:id ,(str "fnrefere" text))
        (a (:href ,(str "#fnreferr" text)) ,text)))

(method MarkdownReader .parse-link2 (text)
  (.skip self "(")
  (while (!= (.next self) ")") (.get self))
  (.skip self ")")
  `(a (:href ,(.token self)) ,text))

(method MarkdownReader .parse-link ()
  (let (caret? nil colon? nil)
    (.skip self "[")
    (when (= (.next self) "^")
      (<- caret? true)
      (.skip self))
    (while (!= (.next self) "]")
      (.get self))
    (.skip self "]")
    (when (= (.next self) ":")
      (<- colon? true)
      (.skip self))
    (apply (if colon? .parse-link0 caret? .parse-link1 .parse-link2)
           (list self (.token self)))))

(method MarkdownReader .parse-string ()
  (let (children nil next nil)
    (while (! (.EOL? self))
      (if (! (in? (<- next (.next self)) '("`" "*" "["))) (.get self)
          (let (text (.token self))
            (if (! (empty? text)) (push! text children))
            (if (= next "`") (push! (.parse-code self) children)
                (= next "*") (push! (.parse-em self) children)
                (= next "[") (push! (.parse-link self) children)
                (assert nil)))))
    (let (text (.token self))
      (if (! (empty? text)) (push! text children))
      (reverse! children))))

(method MarkdownReader .parse-paragraph ()
  `(p () ,@(.parse-string self)))

(method MarkdownReader .parse-pre ()
  (let (get-line (f ()
                   (when (= (.next self) " ")
                     (dotimes (i 4) (.skip self " "))
                     (.skip-line self))))
    `(pre () ,(join (collect get-line) "\n"))))

(method MarkdownReader .parse-quote ()
  (let (next-depth nil node-stack nil
                   fetch (f ()
                           (when (! (.EOL? self))
                             (<- next-depth 0)
                             (while (= (.next self) ">")
                               (.skip self)
                               (<- next-depth (++ next-depth)))
                             (if (= next-depth 0) (raise SyntaxError "missing >"))
                             (.skip-space self)
                             (push! (.skip-line (.skip-space self)) node-stack)))
                   rec (f (depth nodes)
                         (while (fetch)
                           (if (< next-depth depth) (break)
                               (= next-depth depth) (begin
                                                      (if (&& nodes (string? (car nodes))) (push! '(br ()) nodes))
                                                      (push! (pop! node-stack) nodes))
                               (begin
                                 (push! (rec next-depth (list (pop! node-stack))) nodes)
                                 (if node-stack (push! (pop! node-stack) nodes)))))
                         `(blockquote () ,@(reverse! nodes))))
    (rec 1 nil)))

(method MarkdownReader .parse-list ()
  (let (next-root nil next-depth nil node-stack nil
                  fetch (f ()
                          (when (|| (.next? self digit?) (memmem "- " (.next self)))
                            (<- next-depth 1)
                            (while (= (.next self) " ")
                              (dotimes (i 4) (.skip self " "))
                              (<- next-depth (++ next-depth)))
                            (if (= (.next self) "-")
                                (begin
                                  (.skip self)    ; - xxx
                                  (push! 'ul next-root))
                                (.next? self digit?)
                                (begin
                                  (.skip-uint self) (.skip self ".")    ; 1. xxx
                                  (push! 'ol next-root))
                                (raise SyntaxError "missing list"))
                            (.skip-space self)
                            (push! (list 'li () (.skip-line (.skip-space self))) node-stack)))
                  rec (f (root depth nodes)
                        (while (fetch)
                          (if (< next-depth depth) (break)
                              (= next-depth depth) (if (!= root (pop! next-root)) (raise SyntaxError "mixed list type")
                                                       (push! (pop! node-stack) nodes))
                              (begin
                                (push! (rec (pop! next-root) next-depth (list (pop! node-stack))) nodes)
                                (when node-stack
                                  (if (!= (pop! next-root) root) (raise SyntaxError "mixed list type")
                                      (push! (pop! node-stack) nodes))))))
                        (cons root (cons nil (reverse! nodes)))))
    (rec (if (= (.next self) "-") 'ul 'ol) 1 nil)))

(method MarkdownReader .parse-tr (:opt tx)
  (let (txlist nil)
    (.skip self "|")
    (while (!= (.next self) "\n")
      (while (!= (.next self) "|") (.get self))
      (.skip self "|")
      (push! (list tx () (.token self)) txlist))
    (.skip self)
    `(tr () ,@(reverse! txlist))))

(method MarkdownReader .parse-table ()
  (let (thlist (.parse-tr self 'th) tdlist nil)
    (.skip-line self)    ; skip separator.
    (while (= (.next self) "|")
      (push! (.parse-tr self 'td) tdlist))
    `(table ()
            (thead () ,thlist)
            (tbody () ,@(reverse! tdlist)))))

(method MarkdownReader .read ()
  ; Read markdown.
  ; Returns a list representation of read markdown.
  ; Readable markdown is follows.
  ;     <markdown> ::= <stmt> [<stmt> ...]
  ;     <stmt> ::= <stmt_lf> <eol> | <stmt_nolf>
  ;     <stmt_nolf> ::= <header> | <paragraph> | <xml>
  ;     <stmt> ::= <pre> | <quote> | <ul> | <ol> | <table>
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
  ;             | <char>
  ;         } ...
  ;     <code> ::= '`' <char> ... '`'
  ;     <em> ::= '*' <char> ... '*'
  ;     <link> ::= <link0> | <link1> | <link2>
  ;     <link1> :== '[^' <char> ... ']:'
  ;     <link2> :== '[^' <char> ... ']'
  ;     <link0> :== '[' <char> ... '](' <char> ... ')'
  ;     <xml> -- a xml.
  ;     <eol> -- end of line.
  ;     <char> -- characters that have no special meaning.
  (while (= (.next self) "\n") (.skip self))
  (let (next (.next self))
    (if (nil? next) nil
        (= next "#") (.parse-header self)
        (= next " ") (.parse-pre self)
        (= next ">") (.parse-quote self)
        (in? next '("-" "1")) (.parse-list self)
        (= next "|") (.parse-table self)
        (= next "<") (XMLReader.read self)
        (.parse-paragraph self))))

(function! main (args)
  (with-memory-stream ($in "# header1\n")
    (assert (= (.read (.new MarkdownReader)) '(h1 () "header1"))))
  (with-memory-stream ($in "paragraph[^1]\n")
    (assert (= (.read (.new MarkdownReader))
               '(p () "paragraph" (sup (:id "fnrefere1") (a (:href "#fnreferr1") "1"))))))
  (with-memory-stream ($in "link to google [google](https://google.com)\n")
    (assert (= (.read (.new MarkdownReader))
               '(p () "link to google " (a (:href "https://google.com") "google")))))
  (with-memory-stream ($in (join '("|x|y|z|\n"
                                   "||||\n"
                                   "|a|b|c|\n")))
    (assert (= (.read (.new MarkdownReader))
               '(table ()
                       (thead () (tr () (th () "x") (th () "y") (th () "z")))
                       (tbody () (tr () (td () "a") (td () "b") (td () "c")))))))
  (with-memory-stream ($in (join '("    foo\n"
                                   "    bar\n")))
    (assert (= (.read (.new MarkdownReader)) '(pre () "foo\nbar"))))
  (with-memory-stream ($in (join '("> bq1\n"
                                   ">> bq2\n"
                                   "> bq3\n")))
    (assert (= (.read (.new MarkdownReader))
               '(blockquote () "bq1" (blockquote () "bq2") "bq3"))))
  (with-memory-stream ($in (join '("- ul1\n"
                                   "    1. ol1\n"
                                   "    1. ol2\n"
                                   "- ul2\n")))
    (assert (= (.read (.new MarkdownReader))
               '(ul ()
                    (li () "ul1")
                    (ol ()
                        (li () "ol1")
                        (li () "ol2"))
                    (li () "ul2")))))
  (with-memory-stream ($in (join '("<span style='color:red'>foo</span>\n")))
    (assert (= (.read (.new MarkdownReader)) '(span (:style "color:red") "foo"))))
  (with-memory-stream ($in (join '("[^1]: reference\n")))
    (assert (= (.read (.new MarkdownReader))
               '(p nil (a (:id "fnreferr1" :href "#fnrefere1") "[1]") " reference")))))
