; xml module.

(function xml-attrs->string (l)
  (with-memory-stream (out)
    (while l
      (assert (keyword? (car l)))
      (write-bytes " " out)
      (write-bytes (car l) out)
      (if (keyword? (car (<- l (cdr l)))) (continue)
          (assert (string? (car l)))
          (begin
            (write-bytes "='" out)
            (write-bytes (car l) out)
            (write-bytes "'" out)
            (<- l (cdr l)))))))

(function xml->string (l)
  ; Returns a list representation of xml as a string.
  (if (atom? l) l
      (let (name (car l))
        (if (eq? name '?xml) (string "<? " (cadr l) " ?>")
            (eq? name '!DOCTYPE) (string "<!DOCTYPE " (cadr l) ">")
            (eq? name '!--) (string "<!--" (cadr l) "-->")
            (let (attrs (cadr l) children (cddr l))
              (if (! (&& (cons? attrs) (keyword? (car attrs))))
                  (<- children (cons attrs children) attrs nil))
              (string "<" name (xml-attrs->string attrs) ">"
                      (list->string (map xml->string children))
                      "</" name ">"))))))

; reader

(class XMLReader (AheadReader))

(method XMLReader .parse-text ()
  (while (string/= (.next self) "<") (.get self))
  (.token self))

(method XMLReader .parse-attrs ()
  (let (attrs nil q nil)
    (while (&& (string/= (.next self) "/")
               (string/= (.next self) ">"))
      (.skip-space self)
      (while (string/= (.next self) "=")
        (.get self))
      (push! attrs (bytes->keyword (.token self)))
      (.skip self "=")
      (if (! (bytes-index "'\"" (<- q (.skip (.skip-space self))))) (continue))    ; single attribute
      (while (string/= (.next self) q)
        (.get-escape self))
      (.skip self)
      (push! attrs (.token self)))
    (reverse! attrs)))

(method XMLReader .parse-name ()
  (.skip-space self)
  (while (&& (! (.space? self))
             (string/= (.next self) "/")
             (string/= (.next self) ">"))
    (.get self))
  (bytes->symbol (.token self)))

(method XMLReader .parse-?tag ()
  (dostring (c "?xml") (.skip self c))
  (let (attrs (.parse-attrs self))
    (.skip (.skip-space self) "?") (.skip self ">")
    (list '?xml attrs)))

(method XMLReader .parse-doctype ()
  (dostring (c "DOCTYPE") (.skip self c))
  (.skip-space self)
  (while (string/= (.next self) ">") (.get self))
  (.skip self)
  (list '!DOCTYPE (.token self)))

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
    (return (list '!-- (.token self)))))

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
  (if (string/= (.next self) "<") (.parse-text self)
      (begin (.token self)    ; cleanup spaces
             (let ((stag? tag) (.parse-tag self))
               (if (! stag?) tag    ; make sense
                   (let (name (car tag) child nil children nil)
                     (while (neq? name (<- child (.read self)))
                       (if (symbol? child) (.raise self "unexpected close tag " child " expected " name)
                           (push! children child)))
                     (cons (car tag)
                           (cons (cadr tag)
                                 (reverse! children)))))))))

(method XMLReader .read-all ()
  ; Return the result of .read to the EOF as a list.
  (let (node nil nodes nil)
    (while (<- node (.read self))
      (push! nodes node))
    (reverse! nodes)))

(function! main (args)
  (with-memory-stream
    (in (string "<!DOCTYPE html>"
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
                "</html>"))
    (write (map xml->string (write (.read-all (.init (.new XMLReader) in)))))))
