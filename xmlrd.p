; xml reader module.

(import :xml)

(class XML.Reader (AheadReader))

(method XML.Reader .parse-text ()
  (while (string/= (.next self) "<")
    (.get self))
  (&value<- (.new XML.Text) (.token self)))

(method XML.Reader .parse-attrs ()
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
          (error "missing open quote"))
      (while (string/= (.next self) q)
        (.get-escape self))
      (.skip self q)
      (<- val (.token self))
      (push! attrs (bytes->keyword key))
      (push! attrs val))
    (reverse! attrs)))

(method XML.Reader .parse-name ()
  (.skip-space self)
  (while (&& (! (.space? self))
             (string/= (.next self) "/")
             (string/= (.next self) ">"))
    (.get self))
  (bytes->symbol (.token self)))

(method XML.Reader .parse-element ()
  (let (name (.parse-name self) attrs (.parse-attrs self) children nil)
    (.skip self ">")
    (while true
      (.skip-space self))
    (cons name (cons attrs children))))

(method XML.Reader .parse-?tag ()
  (while (string/= (.next self) "?")
    (.get self))
  (.skip self "?")
  (.skip self ">")
  (&value<- (.new XML.Decl) (.token self)))

(method XML.Reader .parse-doctype ()
  (.skip self "D")
  (.skip self "O")
  (.skip self "C")
  (.skip self "T")
  (.skip self "Y")
  (.skip self "P")
  (.skip self "E")
  (.skip-space self)
  (while (string/= (.next self) ">") (.get self))
  (.skip self)
  (&value<- (.new XML.DOCTYPE) (.token self)))

(method XML.Reader .parse-comment ()
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
    (return (&value<- (.new XML.Comment) (.token self)))))

(method XML.Reader .parse-!tag ()
  (.skip self "!")
  (if (string= (.next self) "-") (.parse-comment self)
      (.parse-doctype self)))

(method XML.Reader .parse-tag ()
  (let (stag? nil)
    (.skip self "<")
    (when (string= (.next self) "/")
      (.skip self "/")
      (while (string/= (.next self) ">")
        (.get self))
      (.skip self)
      (return (list stag? (bytes->symbol (.token self)))))
    (if (string= (.next self) "!") (list stag? (.parse-!tag self))
        (string= (.next self) "?") (list stag? (.parse-?tag self))
        (let (name (.parse-name self) attrs (.parse-attrs self))
          (if (string= (.next (.skip-space self)) "/") (.skip self)
              (<- stag? true))
          (.skip self ">")
          (list stag? (&<- (.new XML.Element) :name name :attrs attrs))))))

(method XML.Reader .parse-node ()
  (if (string/= (.next (.skip-space self)) "<") (.parse-text self)
      (let (stag?.val (.parse-tag self) stag? (car stag?.val) val (cadr stag?.val))
        (if (! stag?) val
            (let (stag val child nil children nil)
              (while (neq? (&name stag) (<- child (.parse-node self)))
                (if (symbol? child) (error "missing open tag " child)
                    (push! children child)))
              (&children<- stag (reverse! children)))))))

(method XML.Reader .read ()
  ; Read xml.
  ; Returns the XML.Node object corresponding to the read xml.
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
  (.parse-node self))

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
            rd (.init (.new XML.Reader) xml))
    (write (.read rd))
    (write (.read rd))))
