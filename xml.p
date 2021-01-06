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
                                  (write-mem (string "='" (car l) "'"))
                                  (<- l (cdr l)))
              (assert nil))))))

(function xml->str (l)
  ; Returns a list representation of xml as a string.
  (if (atom? l) l
      (let ((name :opt attrs :rest children) l)
        (if (eq? name '?xml) (string "<? " attrs " ?>")
            (eq? name '!DOCTYPE) (string "<!DOCTYPE " (cadr l) ">")
            (eq? name '!--) (string "<!--" attrs "-->")
            (&& attrs (|| (atom? attrs) (! (keyword? (car attrs)))))
            (string "<" name  ">"
                    (join (map xml->str (cons attrs children)))
                    "</" name ">")
            (string "<" name (xml-attrs->str attrs) ">"
                    (join (map xml->str children))
                    "</" name ">")))))

; reader

(class XMLReader (AheadReader))

(method XMLReader .parse-text ()
  (while (memneq? (.next self) "<") (.get self))
  (.token self))

(method XMLReader .parse-attrs ()
  (let (attrs nil q nil)
    (while (! (memmem  "/>" (.next (.skip-space self))))
      (while (memneq? (.next self) "=") (.get self))
      (push! (mem->key (.token self)) attrs)
      (.skip self "=")
      (if (! (memmem "'\"" (<- q (.skip (.skip-space self))))) (continue))    ; single attribute
      (while (memneq? (.next self) q) (.get-escape self))
      (.skip self)
      (push! (.token self) attrs))
    (reverse! attrs)))

(method XMLReader .parse-name ()
  (.skip-space self)
  (while (&& (! (.space? self))
             (memneq? (.next self) "/")
             (memneq? (.next self) ">"))
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
  (while (memneq? (.next self) ">") (.get self))
  (.skip self)
  (list '!DOCTYPE (.token self)))

(method XMLReader .parse-comment ()
  (.skip self "-")
  (.skip self "-")
  (while true
    (while (memneq? (.next self) "-")
      (.get self))
    (.skip self)
    (when (memneq? (.next self) "-")
      (.put self "-")
      (continue))
    (.skip self)
    (when (memneq? (.next self) ">")
      (.put self "--")
      (continue))
    (.skip self)
    (return (list '!-- (.token self)))))

(method XMLReader .parse-!tag ()
  (.skip self "!")
  (if (memeq? (.next self) "-") (.parse-comment self)
      (.parse-doctype self)))

(method XMLReader .parse-tag ()
  (let (stag? nil)
    (.skip self "<")
    (when (memeq? (.next self) "/")
      (.skip self "/")
      (while (memneq? (.next self) ">")
        (.get self))
      (.skip self)
      (return (list stag? (mem->sym (.token self)))))    ; return etag symbol.
    (if (memeq? (.next self) "!") (list stag? (.parse-!tag self))
        (memeq? (.next self) "?") (list stag? (.parse-?tag self))
        (let (name (.parse-name self) attrs (.parse-attrs self))
          (if (memeq? (.next (.skip-space self)) "/") (.skip self)
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
  (if (memneq? (.next self) "<") (.parse-text self)
      (begin (.token self)    ; cleanup spaces
             (let ((stag? tag) (.parse-tag self))
               (if (! stag?) tag    ; make sense
                   (let (name (car tag) child nil children nil)
                     (while (neq? name (<- child (XMLReader.read self)))
                       (if (symbol? child) (.raise self "unexpected close tag " child " expected " name)
                           (push! child children)))
                     (cons (car tag)
                           (cons (cadr tag)
                                 (reverse! children)))))))))

(function! main (args)
  (assert (memeq? (xml->str '(html (:lang "ja") "foo")) "<html lang='ja'>foo</html>"))
  (assert (memeq? (xml->str '(title ())) "<title></title>"))
  (with-memory-stream ($in "<!DOCTYPE html>")
    (let (rd (.new XMLReader) (doctype text) (.read rd))
      (assert (eq? doctype '!DOCTYPE))
      (assert (memeq? text "html"))))
  (with-memory-stream ($in "<!-- -- foo -- -->")
    (let (rd (.new XMLReader) (comment text) (.read rd))
      (assert (eq? comment '!--))
      (assert (memeq? text " -- foo -- "))))
  (with-memory-stream ($in "<div></div>")
    (let (rd (.new XMLReader) (div div-attr) (.read rd))
      (assert (eq? div 'div))
      (assert (nil? div-attr))))
  (with-memory-stream ($in "<input type='hidden'/>")
    (let (rd (.new XMLReader) (input (type type-val)) (.read rd))
      (assert (eq? input 'input))
      (assert (eq? type :type))
      (assert (memeq? type-val "hidden"))))
  (with-memory-stream ($in (join '("<ul>"
                                   "    <li>foo</li>"
                                   "    <li>bar</li>"
                                   "</ul>")))
    (let (rd (.new XMLReader) (ul ul-attr
                                  (li1 li1-attr li1-text)
                                  (li2 li2-attr li2-text)) (.read rd))
      (assert (eq? ul 'ul))
      (assert (eq? li1 li2 'li))
      (assert (memeq? li1-text "foo"))
      (assert (memeq? li2-text "bar")))))
