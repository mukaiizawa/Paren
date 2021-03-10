; xml module.

(function xml-attrs->str (l)
  (if l
      (with-memory-stream ($out)
        (while l
          (assert (keyword? (car l)))
          (write-mem " ")
          (write-mem (car l))
          (if (keyword? (car (<- l (cdr l)))) (continue)
              (string? (car l)) (begin
                                  (write-mem (str "='" (car l) "'"))
                                  (<- l (cdr l)))
              (assert nil))))))

(function xml->str (l)
  ; Returns a list representation of xml as a string.
  (if (atom? l) l
      (let ((name :opt attrs :rest children) l)
        (if (== name '?xml) (str "<? " attrs " ?>")
            (== name '!DOCTYPE) (str "<!DOCTYPE " (cadr l) ">")
            (== name '!--) (str "<!--" attrs "-->")
            (&& attrs (|| (atom? attrs) (! (keyword? (car attrs)))))
            (str "<" name  ">"
                 (join (map xml->str (cons attrs children)))
                 "</" name ">")
            (str "<" name (xml-attrs->str attrs) ">"
                 (join (map xml->str children))
                 "</" name ">")))))

; reader

(class XMLReader (AheadReader))

(method XMLReader .parse-text ()
  (while (!= (.next self) "<") (.get self))
  (.token self))

(method XMLReader .parse-attrs ()
  (let (attrs nil q nil)
    (while (! (memmem  "/>" (.next (.skip-space self))))
      (while (!= (.next self) "=") (.get self))
      (push! (mem->key (.token self)) attrs)
      (.skip self "=")
      (if (! (memmem "'\"" (<- q (.skip (.skip-space self))))) (continue))    ; single attribute
      (while (!= (.next self) q) (.get-escape self))
      (.skip self)
      (push! (.token self) attrs))
    (reverse! attrs)))

(method XMLReader .parse-name ()
  (.skip-space self)
  (while (&& (! (.space? self))
             (!= (.next self) "/")
             (!= (.next self) ">"))
    (.get self))
  (mem->sym (.token self)))

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
  (while true
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
      (return (list stag? (mem->sym (.token self)))))    ; return etag symbol.
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
  (if (nil? (.next self)) (return nil))
  (while (.space? self) (.get self))
  (if (!= (.next self) "<") (.parse-text self)
      (begin (.token self)    ; cleanup spaces
             (let ((stag? tag) (.parse-tag self))
               (if (! stag?) tag    ; make sense
                   (let (name (car tag) child nil children nil)
                     (while (!= name (<- child (XMLReader.read self)))
                       (if (symbol? child) (.raise self "unexpected close tag " child " expected " name)
                           (push! child children)))
                     (cons (car tag)
                           (cons (cadr tag)
                                 (reverse! children)))))))))

(function! main (args)
  (assert (= (xml->str '(html (:lang "ja") "foo")) "<html lang='ja'>foo</html>"))
  (assert (= (xml->str '(title ())) "<title></title>"))
  (with-memory-stream ($in "<!DOCTYPE html>")
    (assert (= (.read (.new XMLReader)) '(!DOCTYPE "html"))))
  (with-memory-stream ($in "<!-- -- foo -- -->")
    (assert (= (.read (.new XMLReader)) '(!-- " -- foo -- "))))
  (with-memory-stream ($in "<div></div>")
    (assert (= (.read (.new XMLReader)) '(div ()))))
  (with-memory-stream ($in "<input type='hidden'/>")
    (assert (= (.read (.new XMLReader)) '(input (:type "hidden")))))
  (with-memory-stream ($in (str "<ul>"
                                "    <li>foo</li>"
                                "    <li>bar</li>"
                                "</ul>"))
    (assert (= (.read (.new XMLReader)) '(ul ()
                                             (li () "foo")
                                             (li () "bar"))))))
