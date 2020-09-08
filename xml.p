; xml module.

; class

(class XMLNode ())
(class XMLNode.DOCTYPE (XMLNode) value)
(class XMLNode.Decl (XMLNode) value)
(class XMLNode.Comment (XMLNode) value)
(class XMLNode.Text (XMLNode) value)
(class XMLNode.Element (XMLNode) name attrs children)

(method XMLNode .to-s ()
  (assert nil))

(method XMLNode.DOCTYPE .to-s ()
  (string "<!DOCTYPE " (&value self) ">"))

(method XMLNode.Decl .to-s ()
  (string "<? " (&value self) " ?>"))

(method XMLNode.Comment .to-s ()
  (string "<!--" (&value self) "-->"))

(method XMLNode.Text .to-s ()
  (&value self))

(method XMLNode.Element .to-s ()
  (string "<" (&name self) (list->string (map (lambda (attr)
                                                (let (key (bytes->string (car attr)) val (cadr attr))
                                                  (if val (string " " key "='" val "'")
                                                      (string " " key))))
                                              (group (&attrs self) 2))) ">"
          (list->string (map .to-s (&children self)))
          "</" (&name self) ">"))

; reader

(class XMLReader (AheadReader))

(method XMLReader .parse-text ()
  (while (string/= (.next self) "<")
    (.get self))
  (&value<- (.new XMLNode.Text) (.token self)))

(method XMLReader .parse-attrs ()
  (let (attrs nil key nil q nil val nil)
    (while (&& (string/= (.next self) "/")
               (string/= (.next self) ">"))
      (.skip-space self)
      (while (string/= (.next self) "=")
        (.get self))
      (<- key (.token self))
      (.skip self "=")
      (if (&& (string/= (<- q (.skip (.skip-space self))) "\"")
              (string/= q "'"))
          (.raise self "missing open quote"))
      (while (string/= (.next self) q)
        (.get-escape self))
      (.skip self q)
      (<- val (.token self))
      (push! attrs (bytes->keyword key))
      (push! attrs val))
    (reverse! attrs)))

(method XMLReader .parse-name ()
  (.skip-space self)
  (while (&& (! (.space? self))
             (string/= (.next self) "/")
             (string/= (.next self) ">"))
    (.get self))
  (bytes->symbol (.token self)))

(method XMLReader .parse-?tag ()
  (while (string/= (.next self) "?")
    (.get self))
  (.skip self "?")
  (.skip self ">")
  (&value<- (.new XMLNode.Decl) (.token self)))

(method XMLReader .parse-doctype ()
  (dostring (c "DOCTYPE") (.skip self c))
  (.skip-space self)
  (while (string/= (.next self) ">") (.get self))
  (.skip self)
  (&value<- (.new XMLNode.DOCTYPE) (.token self)))

(method XMLReader .parse-comment ()
  (.skip self "-")
  (.skip self "-")
  (while true
    (while (string/= (.next self) "-")
      (.get self))
    (.skip self)
    (when (string/= (.next self) "-")
      (.put self "-")
      (continue))
    (.skip self)
    (when (string/= (.next self) ">")
      (.put self "--")
      (continue))
    (.skip self)
    (return (&value<- (.new XMLNode.Comment) (.token self)))))

(method XMLReader .parse-!tag ()
  (.skip self "!")
  (if (string= (.next self) "-") (.parse-comment self)
      (.parse-doctype self)))

(method XMLReader .parse-tag ()
  (let (stag? nil)
    (.skip self "<")
    (when (string= (.next self) "/")
      (.skip self "/")
      (while (string/= (.next self) ">")
        (.get self))
      (.skip self)
      (return (list stag? (bytes->symbol (.token self)))))    ; return etag symbol.
    (if (string= (.next self) "!") (list stag? (.parse-!tag self))
        (string= (.next self) "?") (list stag? (.parse-?tag self))
        (let (name (.parse-name self) attrs (.parse-attrs self))
          (if (string= (.next (.skip-space self)) "/") (.skip self)
              (<- stag? true))
          (.skip self ">")
          (list stag? (&<- (.new XMLNode.Element) :name name :attrs attrs))))))

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
  (while (.space? self) (.get self))
  (if (string/= (.next self) "<") (.parse-text self)
      (begin (.token self)    ; cleanup spaces
             (let (stag?.val (.parse-tag self) stag? (car stag?.val) val (cadr stag?.val))
               (if (! stag?) val    ; make sense
                   (let (stag val name (&name stag) child nil children nil)
                     (while (neq? name (<- child (.read self)))
                       (if (symbol? child) (.raise self "unexpected close tag " child " expected " name)
                           (push! children child)))
                     (&children<- stag (reverse! children))))))))

(function! main (args)
  (let (xml (string 
              "<!DOCTYPE html>"
              "<html lang='ja'>"
              "    <head>"
              "        <title>foo</title>"
              "    </head>"
              "<!-- -- comment -- -->"
              "    <body>"
              "         <hr/>"
              "         <div style='bar'"
              "              class='buzz'>"
              "           text node"
              "           text node"
              "         </div>"
              "         <hr/>"
              "    </body>"
              "</html>")
            rd (.init (.new XMLReader) xml))
    (write (.read rd))
    (write (.read rd))))
