; xml module.

(function xml.write-text (x)
  (with-memory-stream ($in x)
    (let (ch nil)
      (while (<- ch (read-char))
        (write-bytes
          (if (= ch "\"") "&quot;"
              (= ch "'") "&apos;"
              (= ch "<") "&lt;"
              (= ch ">") "&gt;"
              (= ch "&") "&amp;"
              ch))))))

(function xml.read-text (x)
  (with-memory-stream ($out)
    (with-memory-stream ($in x)
      (let (ch nil rd (.new AheadReader))
        (while (<- ch (.next rd))
          (if (!= ch "&") (write-bytes (.skip rd))
              (begin
                (while (!= (.next rd) ";") (.get rd))
                (.get rd)
                (let (tk (.token rd))
                  (write-bytes
                    (if (= tk "&quot;") "\""
                        (= tk "&apos;") "'"
                        (= tk "&lt;") "<"
                        (= tk "&gt;") ">"
                        (= tk "&amp;") "&"
                        tk))))))))))

(function xml.write1 (x)
  (if (nil? x) nil
      (string? x) (xml.write-text x)
      (assert nil)))

(function xml.write-attr (attrs)
  (while attrs
    (assert (keyword? (car attrs)))
    (write-bytes " ")
    (write-bytes (car attrs))
    (let (next (car (<- attrs (cdr attrs))))
      (if (nil? next) (break)
          (keyword? next) (continue)
          (string? next) (begin (foreach write-bytes (list "='" next "'")) (<- attrs (cdr attrs)))
          (assert nil)))))

(function xml.write (x)
  ; Returns a list representation of xml as a string.
  (if (atom? x) (xml.write1 x)
      (let ((name :opt attrs :rest children) x)
        (if (= name '?xml) (foreach write-bytes (list "<? " attrs " ?>"))
            (= name '!DOCTYPE) (foreach write-bytes (list "<!DOCTYPE " attrs ">"))
            (= name '!--) (foreach write-bytes (list "<!--" attrs "-->"))
            (&& attrs (|| (atom? attrs) (! (keyword? (car attrs)))))
            (begin
              (foreach write-bytes (list "<" name  ">"))
              (foreach xml.write (cons attrs children))
              (foreach write-bytes (list "</" name  ">")))
            (begin
              (write-bytes "<") (write-bytes name) (xml.write-attr attrs) (write-bytes ">")
              (foreach xml.write children)
              (foreach write-bytes (list "</" name  ">")))))))

(function xml->str (x)
  (with-memory-stream ($out)
    (xml.write x)))

; reader

(class XMLReader (AheadReader))

(method XMLReader .parse-text ()
  (while (!= (.next self) "<") (.get self))
  (xml.read-text (.token self)))

(method XMLReader .parse-attrs ()
  (let (attrs nil q nil)
    (while (! (in? (.next (.skip-space self)) '("/" ">")))
      (while (! (in? (.next self) '("=" " " "/" ">")))
        (.get self))
      (push! (keyword (.token self)) attrs)
      (if (!= (.next (.skip-space self)) "=") (continue)    ; single attribute
          (.skip self "="))
      (if (! (in? (<- q (.skip (.skip-space self))) '("'" "\"")))
          (raise StateError "missing attribute value"))
      (while (!= (.next self) q)
        (.get-escape self))
      (.skip self q)
      (push! (.token self) attrs))
    (reverse! attrs)))

(method XMLReader .parse-name ()
  (.skip-space self)
  (while (&& (! (.next? self space?))
             (!= (.next self) "/")
             (!= (.next self) ">"))
    (.get self))
  (symbol (.token self)))

(method XMLReader .parse-?tag ()
  (dostring (c "?xml") (.skip self c))
  (let (attrs (.parse-attrs self))
    (.skip (.skip-space self) "?") (.skip self ">")
    (list '?xml attrs)))

(method XMLReader .parse-doctype ()
  (dostring (c "DOCTYPE") (.skip self c))
  (.skip-space self)
  (while (!= (.next self) ">") (.get self))
  (.skip self)
  (list '!DOCTYPE (.token self)))

(method XMLReader .parse-comment ()
  (.skip self "-")
  (.skip self "-")
  (loop
    (while (!= (.next self) "-")
      (.get self))
    (.skip self)
    (when (!= (.next self) "-")
      (.put self "-")
      (continue))
    (.skip self)
    (when (!= (.next self) ">")
      (.put self "--")
      (continue))
    (.skip self)
    (return (list '!-- (.token self)))))

(method XMLReader .parse-!tag ()
  (.skip self "!")
  (if (= (.next self) "-") (.parse-comment self)
      (.parse-doctype self)))

(method XMLReader .parse-tag ()
  (let (stag? nil)
    (.skip self "<")
    (when (= (.next self) "/")
      (.skip self "/")
      (while (!= (.next self) ">") (.get self))
      (.skip self)
      (return (list stag? (symbol (.token self)))))    ; return etag symbol.
    (if (= (.next self) "!") (list stag? (.parse-!tag self))
        (= (.next self) "?") (list stag? (.parse-?tag self))
        (let (name (.parse-name self) attrs (.parse-attrs self))
          (if (= (.next (.skip-space self)) "/") (.skip self)
              (<- stag? true))
          (.skip self ">")
          (list stag? (list name attrs))))))

(method XMLReader .read ()
  ; Read xml.
  ; Returns the XMLNode object corresponding to the read xml.
  ; Readable xml is as follows.
  ;     <xml> ::= <node> ...
  ;     <node> ::= <tag>
  ;              | <text>
  ;     <tag> ::= <stag> <node> ... <etag>
  ;             | <singletag>
  ;             | <?tag>
  ;             | <!tag>
  ;     <?tag> :== '<?'	 <char> ... '>'
  ;     <!tag> :== <doctype>
  ;              | <comment>
  ;     <doctype> ::= '<!DOCTYPE' <char> ... '>'
  ;     <comment> ::= '<!--' <char> ... '-->'
  ;     <element> ::= <single_element>
  ;                 | <open_element> <node> ... <close_element>
  ;     <singletag> ::= '<' <name> [<attr> ...] '/>'
  ;     <stag> ::= '<' <name> [<attr> ...] '>'
  ;     <etag> ::= '</' <name>   '>'
  ;     <attr> ::= <key> ['=' '"' <value> '"']
  ;     <text> -- a text node.
  (while (.next? self space?) (.get self))
  (if (nil? (.next self)) (return nil)
      (!= (.next self) "<") (.parse-text self)
      (begin
        (.token self)    ; cleanup spaces
        (let ((stag? tag) (.parse-tag self))
          (if (! stag?) tag    ; make sense
              (let (name (car tag) child nil children nil)
                (while (!= name (<- child (XMLReader.read self)))
                  (if (symbol? child) (raise StateError (str "unexpected close tag " child " expected " name))
                      (push! child children)))
                (cons (car tag)
                      (cons (cadr tag)
                            (reverse! children)))))))))

(function! main (args)
  (assert (= (xml->str '(html (:lang "ja") "foo")) "<html lang='ja'>foo</html>"))
  (assert (= (xml->str '(script (:async :src "foo"))) "<script async src='foo'></script>"))
  (assert (= (xml->str '(script (:src "foo" :async))) "<script src='foo' async></script>"))
  (assert (= (xml->str '(title ())) "<title></title>"))
  (with-memory-stream ($in "<!DOCTYPE html>")
    (assert (= (.read (.new XMLReader)) '(!DOCTYPE "html"))))
  (with-memory-stream ($in "<!-- -- foo -- -->")
    (assert (= (.read (.new XMLReader)) '(!-- " -- foo -- "))))
  (with-memory-stream ($in "<div></div>")
    (assert (= (.read (.new XMLReader)) '(div ()))))
  (with-memory-stream ($in "<input type='hidden'/>")
    (assert (= (.read (.new XMLReader)) '(input (:type "hidden")))))
  (with-memory-stream ($in "<script async src='foo'/>")
    (assert (= (.read (.new XMLReader)) '(script (:async :src "foo")))))
  (with-memory-stream ($in "<script src='foo'async/>")
    (assert (= (.read (.new XMLReader)) '(script (:src "foo" :async)))))
  (with-memory-stream ($in (str "<ul>"
                                "    <li>foo</li>"
                                "    <li>bar</li>"
                                "</ul>"))
    (assert (= (.read (.new XMLReader)) '(ul ()
                                             (li () "foo")
                                             (li () "bar"))))))
