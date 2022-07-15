; xml module.

(class XMLError (Error))

;; Reader, Parser.

(class XMLReader (AheadReader))

(method XMLReader .reset ()
  (.token self)
  self)

(method XMLReader .get-space ()
  (while (.next? self space?) (.get self))
  self)

(method XMLReader .read-text ()
  (while (!= (.next self) "<")
    (.get self))
  (with-memory-stream ($out)
    (with-memory-stream ($in (.token self))
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

(method XMLReader .read-attrs ()
  (let (attrs nil q nil)
    (while (! (in? (.next (.skip-space self)) '("?" "/" ">")))
      (while (! (in? (.next self) '("=" " " "/" ">")))
        (.get self))
      (push! (keyword (.token self)) attrs)
      (if (!= (.next (.skip-space self)) "=") (continue)    ; single attribute
          (.skip self "="))
      (if (! (in? (<- q (.skip (.skip-space self))) '("'" "\"")))
          (raise XMLError "missing attribute value"))
      (while (!= (.next self) q)
        (.get-escape self))
      (.skip self q)
      (push! (.token self) attrs))
    (reverse! attrs)))

(method XMLReader .read-name ()
  (.skip-space self)
  (while (&& (! (.next? self space?))
             (!= (.next self) "/")
             (!= (.next self) ">"))
    (.get self))
  (symbol (.token self)))

(method XMLReader .read-declaration ()
  (dostring (ch "?xml")
    (.skip self ch))
  (let (attrs (.read-attrs self))
    (.skip (.skip-space self) "?") (.skip self ">")
    (list '?xml attrs)))

(method XMLReader .read-doctype ()
  (dostring (ch "DOCTYPE")
    (if (= (.next self) ch) (.skip self)
        (.skip self (lower ch))))
  (.skip-space self)
  (while (!= (.next self) ">")
    (.get self))
  (.skip self)
  (list '!DOCTYPE (.token self)))

(method XMLReader .read-comment ()
  (dostring (ch "--") (.skip self ch))
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

(method XMLReader .read-exclam ()
  (.skip self "!")
  (if (= (.next self) "-") (.read-comment self)
      (.read-doctype self)))

(method XMLReader .read-tag ()
  (.skip self "<")
  (if (= (.next self) "!") (list nil (.read-exclam self))
      (= (.next self) "?") (list nil (.read-declaration self))
      (= (.next self) "/") (begin
                             (.skip self "/")
                             (while (!= (.next self) ">") (.get self))
                             (.skip self)
                             (list nil (symbol (.token self))))    ; end-element
      (let (start-element? nil name (.read-name self) attrs (.read-attrs self))
        (if (= (.next (.skip-space self)) "/") (.skip self)
            (<- start-element? true))
        (.skip self ">")
        (list start-element? (list name attrs)))))

(method XMLReader .read ()
  ; Read xml.
  ; Returns the list corresponding to dom.
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
  (if (nil? (.next (.get-space self))) nil
      (!= (.next self) "<") (.read-text self)
      (let ((start-element? tag) (.read-tag (.reset self)))
        (if (! start-element?) tag    ; make sense
            (let (name (car tag) child nil children nil)
              (while (!= name (<- child (XMLReader.read self)))
                (if (symbol? child) (raise XMLError (str "unexpected close tag " child " expected " name))
                    (push! child children)))
              (concat tag (reverse! children)))))))

(class XMLSAXParser (AheadReader XMLReader)
  on-start-element
  on-end-element
  on-read-text
  on-read-comment
  on-error)

(method XMLSAXParser .init (:key on-start-element on-end-element on-read-text on-read-comment on-error)
  (<- self->on-start-element (|| on-start-element (f (name attrs) nil))
      self->on-end-element (|| on-end-element (f (name) nil))
      self->on-read-text (|| on-read-text (f (text) nil))
      self->on-read-comment (|| on-read-comment (f (comment) nil))
      self->on-error (|| on-error (f (parser error) (throw error))))
  (AheadReader.init self))

(method XMLSAXParser .parse ()
  ;; parse xml from standard input.
  (catch (Error (partial self->on-error self))
    (while (.next (.get-space self))
      (if (!= (.next self) "<") (apply self->on-read-text (list (.read-text self)))
          (let ((_ tag) (.read-tag (.reset self)))
            (if (symbol? tag) (apply self->on-end-element (list tag))
                (in? (car tag) '(!DOCTYPE ?xml)) (continue)
                (== (car tag) '!--) (apply self->on-read-comment (cdr tag))
                (apply self->on-start-element tag)))))))

;; Wirter.

(function xml.write (x)
  (let (write-attr (f (x)
                     (if (nil? x) nil
                         (! (cons? x)) (raise XMLError "attributes must be list")
                         (let (rest x curr (car x))
                           (while rest
                             (if (! (keyword? curr)) (raise XMLError "attribute name must be keyword")
                                 (begin
                                   (write-bytes " ")
                                   (write-bytes curr)
                                   (<- rest (cdr rest)
                                       curr (car rest))
                                   (if (nil? curr) (break)
                                       (keyword? curr) (continue)
                                       (string? curr) (begin
                                                        (foreach write-bytes (list "='" curr "'"))
                                                        (<- rest (cdr rest)
                                                            curr (car rest)))
                                       (raise XMLError "attribute value must be string"))))))))
                   write1 (f (x)
                            (if (string? x) (dostring (ch x)
                                              (write-bytes (if (= ch "\"") "&quot;"
                                                               (= ch "'") "&apos;"
                                                               (= ch "<") "&lt;"
                                                               (= ch ">") "&gt;"
                                                               (= ch "&") "&amp;"
                                                               ch)))
                                (cons? x) (let ((name :opt attrs :rest children) x)
                                            (if (= name '?xml) (begin (write-bytes "<?xml") (write-attr attrs) (write-bytes "?>"))
                                                (= name '!DOCTYPE) (foreach write-bytes (list "<!DOCTYPE " attrs ">"))
                                                (= name '!--) (foreach write-bytes (list "<!--" attrs "-->"))
                                                (begin
                                                  (write-bytes "<") (write-bytes name) (write-attr attrs) (write-bytes ">")
                                                  (foreach write1 children)
                                                  (foreach write-bytes (list "</" name  ">")))))
                                (raise XMLError "unexpected expression"))))
    (write1 x)
    (write-line)
    x))

(function! main (args)
  ;; DOM.
  (with-memory-stream ($in "<!DOCTYPE html>")
    (assert (= (.read (.new XMLReader))
               '(!DOCTYPE "html"))))
  (with-memory-stream ($in "<?xml version='1.0' encoding='UTF-8' standalone='yes'?>")
    (assert (= (.read (.new XMLReader))
               '(?xml (:version "1.0" :encoding "UTF-8" :standalone "yes")))))
  (with-memory-stream ($in "<!-- -- foo -- -->")
    (assert (= (.read (.new XMLReader))
               '(!-- " -- foo -- "))))
  (with-memory-stream ($in "<div></div>")
    (assert (= (.read (.new XMLReader))
               '(div ()))))
  (with-memory-stream ($in "<input type='hidden'/>")
    (assert (= (.read (.new XMLReader))
               '(input (:type "hidden")))))
  (with-memory-stream ($in "<script async src='foo'/>")
    (assert (= (.read (.new XMLReader))
               '(script (:async :src "foo")))))
  (with-memory-stream ($in "<script src='foo'async/>")
    (assert (= (.read (.new XMLReader))
               '(script (:src "foo" :async)))))
  (with-memory-stream ($in (str "<ul>"
                                "    <li>foo</li>"
                                "    <li>bar</li>"
                                "</ul>"))
    (assert (= (.read (.new XMLReader))
               '(ul ()
                    (li () "foo")
                    (li () "bar")))))
  ;; SAX.
  (with-memory-stream ($in (str "<img src='foo'/>"
                                "<!-- contents start -->"
                                "<div class='contents'>"
                                "    <ul class='none'>"
                                "        <li>foo</li>"
                                "        <li>bar</li>"
                                "    </ul>"
                                "</div>"))
    (let (elements nil classes nil texts (.new MemoryStream) comment nil)
      (.parse (.init (.new XMLSAXParser)
                     :on-read-text (f (text) (.write-bytes texts text))
                     :on-read-comment (f (text) (<- comment text))
                     :on-start-element (f (name attrs)
                                         (let (class (cadr (member :class attrs)))
                                           (if class (push! class classes))))
                     :on-end-element (f (name) (push! name elements))))
      (assert (= (.to-s texts) "foobar"))
      (assert (= comment " contents start "))
      (assert (= (sort! (uniq elements)) (sort! '(div ul li))))
      (assert (= (sort! classes) (sort! '("contents" "none"))))))
  ;; Writer.
  (assert (= (with-memory-stream ($out)
               (xml.write '(!DOCTYPE "html")))
             "<!DOCTYPE html>\n"))
  (assert (= (with-memory-stream ($out)
               (xml.write '(?xml (:version "1.0" :encoding "UTF-8" :standalone "yes"))))
             "<?xml version='1.0' encoding='UTF-8' standalone='yes'?>\n"))
  (assert (= (with-memory-stream ($out)
               (xml.write '(html (:lang "ja") "foo")))
             "<html lang='ja'>foo</html>\n"))
  (assert (= (with-memory-stream ($out)
               (xml.write '(!-- " -- foo -- ")))
             "<!-- -- foo -- -->\n"))
  (assert (= (with-memory-stream ($out)
               (xml.write '(script (:async :src "foo"))))
             "<script async src='foo'></script>\n"))
  (assert (= (with-memory-stream ($out)
               (xml.write '(script (:src "foo" :async))))
             "<script src='foo' async></script>\n"))
  (assert (= (with-memory-stream ($out)
               (xml.write '(title ())))
             "<title></title>\n")))
