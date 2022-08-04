; xml module.

;; Reader, Parser.

(class XML.Reader ()
  stream)

(method XML.Reader .init ()
  (<- self->stream (.new MemoryStream))
  (.write-bytes self->stream (.read-bytes (dynamic $in)))
  self)

(method XML.Reader .next ()
  (.peek-char self->stream))

(method XML.Reader .skip ()
  (.read-char self->stream))

(method XML.Reader .skip-size (size)
  (dotimes (i size)
    (.skip self))
  self)

(method XML.Reader .skip-space ()
  (let (ch nil)
    (while (<- ch (.next self))
      (if (space? ch) (.skip self)
          (break)))
    self))

(method XML.Reader .match? (text)
  (let (match? true pos (.tell self->stream))
    (dostring (ch text)
      (when (!= ch (.skip self))
        (<- match? nil)
        (break)))
    (.seek self->stream pos)
    match?))

(method XML.Reader .read-ident ()
  (let (ident (.new MemoryStream))
    (while (alnum? (.next self))
      (.write-bytes ident (.skip self)))
    (assert (! (empty? (.to-s ident))))
    (symbol (.to-s ident))))

(method XML.Reader .read-quoted ()
  (with-memory-stream (val)
    (let (ch nil q (.skip self))
      (if (! (in? q '("'" "\""))) (raise SyntaxError "expected quote")
          (while (!= (<- ch (.skip self)) q)
            (if (nil? ch) (raise EOFError "missing quote")
                (.write-bytes val ch)))))))

(method XML.Reader .read-text ()
  (with-memory-stream (text)
    (let (ch nil)
      (while (<- ch (.next self))
        (if (= ch "<") (break)
            (!= ch "&") (.write-bytes text (.skip self))
            (.write-bytes text
                          (if (.match? self "&quot;") (begin (.skip-size self 6) "\"")
                              (.match? self "&apos;") (begin (.skip-size self 6) "'")
                              (.match? self "&lt;")   (begin (.skip-size self 4) "<")
                              (.match? self "&gt;")   (begin (.skip-size self 4) ">")
                              (.match? self "&amp;")  (begin (.skip-size self 5) "&")
                              (.skip self))))))))

(method XML.Reader .read-attrs ()
  (let (ch nil attrs nil)
    (while (! (in? (<- ch (.next (.skip-space self))) '("/" ">")))
      (if (nil? ch) (raise EOFError "missing '>'")
          (push! (keyword (.read-ident self)) attrs))
      (if (!= (.skip (.skip-space self)) "=") (raise SyntaxError "missing '='")
          (push! (.read-quoted (.skip-space self)) attrs)))
    (reverse! attrs)))

(method XML.Reader .skip-declaration ()
  (if (.match? self "<?xml") (while (!= (.skip self) ">"))))

(method XML.Reader .read-comment ()
  (if (! (.match? self "!--")) (raise SyntaxError "invalid comment")
      (let (ch nil comment (.new MemoryStream))
        (.skip-size self 3)
        (while (! (.match? self "-->"))
          (if (nil? (<- ch (.skip self))) (raise EOFError "comment not terminated")
              (.write-bytes comment ch)))
        (.skip-size self 3)
        (.to-s comment))))

(method XML.Reader .read-tag ()
  (.skip self)    ; <
  (let (ch (.next self))
    (if (= ch "!") (list :comment (.read-comment self))
        (= ch "/") (begin
                     (.skip self)
                     (let (name (.read-ident self))
                       (if (!= (.skip self) ">") (raise SyntaxError (str "missing close tag " name " '>'"))
                           (list :close name))))
        (let (type :open name (.read-ident self) attrs (.read-attrs self))
          (<- ch (.skip (.skip-space self)))
          (if (= ch "/") (<- type :single ch (.skip self)))
          (if (!= ch ">") (raise SyntaxError (str "missing close tag " (list name attrs) " '>'"))
              (list type (list name attrs)))))))

(method XML.Reader .text-node? ()
  (let (ch nil pos (.tell self->stream) text? true)
    (while (<- ch (.skip self))
      (if (space? ch) (continue)
          (begin
            (<- text? (!= ch "<"))
            (break))))
    (.seek self->stream pos)
    text?))

(method XML.Reader .read-element ()
  (if (nil? (.next self)) nil
      (.text-node? self) (.read-text self)
      (let ((type val) (.read-tag (.skip-space self)))
        (if (== type :comment) (.read-element self)
            (in? type '(:close :single)) val    ; make sense
            (let (name (car val) node nil children nil)
              (while (!== name (<- node (.read-element self)))
                (if (symbol? node) (raise SyntaxError (str "unexpected close tag " child " expected " name))
                    (push! node children)))
              (concat val (reverse! children)))))))

(method XML.Reader .read ()
  (.skip-declaration self)
  (.read-element self))

(function xml.read ()
  ; Read xml.
  ; Returns the list corresponding to xml.
  (.read (.new XML.Reader)))

(class XML.SAXParser (Object XML.Reader)
  on-start-element
  on-end-element
  on-read-text
  on-read-comment
  on-error)

(method XML.SAXParser .init (:key on-start-element on-end-element on-read-text on-read-comment on-error)
  (<- self->on-start-element (|| on-start-element (f (name attrs) nil))
      self->on-end-element (|| on-end-element (f (name) nil))
      self->on-read-text (|| on-read-text (f (text) nil))
      self->on-read-comment (|| on-read-comment (f (comment) nil))
      self->on-error (|| on-error (f (parser error) (throw error))))
  (XML.Reader.init self))

(method XML.SAXParser .parse ()
  ;; parse xml from standard input.
  (catch (Error (partial self->on-error self))
    (.skip-declaration self)
    (while (.next self)
      (if (.text-node? self) (apply self->on-read-text (list (.read-text self)))
          (let ((type val) (.read-tag (.skip-space self)))
            (if (== type :close) (apply self->on-end-element (list val))
                (== type :comment) (apply self->on-read-comment (list val))
                (apply self->on-start-element val)))))))

;; Wirter.

(function xml.write-element (x)
  (let (write-attr (f (x)
                     (if (! (list? x)) (raise SyntaxError "attributes must be list")
                         (map-group (f (k v)
                                      (if (! (keyword? k)) (raise SyntaxError "attribute name must be keyword")
                                          (! (string? v)) (raise SyntaxError "attribute value must be string")
                                          (foreach write-bytes
                                                   (list " " k "='" v "'"))))
                                    x 2)))
                   write1 (f (x)
                            (if (nil? x) nil
                                (string? x) (dostring (ch x)
                                              (write-bytes (if (= ch "\"") "&quot;"
                                                               (= ch "'") "&apos;"
                                                               (= ch "<") "&lt;"
                                                               (= ch ">") "&gt;"
                                                               (= ch "&") "&amp;"
                                                               ch)))
                                (cons? x) (let ((name :opt attrs :rest children) x)
                                            (write-bytes "<") (write-bytes name) (write-attr attrs) (write-bytes ">")
                                            (foreach write1 children)
                                            (write-bytes "</") (write-bytes name) (write-bytes ">"))
                                (raise SyntaxError "unexpected expression"))))
    (write1 x)
    (write-line)
    x))

(function xml.write (x)
  (write-line "<?xml version='1.0' encoding='UTF-8'?>")
  (xml.write-element x))

(function! main (args)
  ;; Reader.
  (assert (= (with-memory-stream ($in "<?xml version='1.0'?>\n<a>bar</a>")
               (xml.read))
             '(a () "bar")))
  (assert (= (with-memory-stream ($in "<a><!-- -- foo -- -->bar</a>")
               (xml.read))
             '(a () "bar")))
  (assert (= (with-memory-stream ($in "<a>bar</a>")
               (xml.read))
             '(a () "bar")))
  (assert (= (with-memory-stream ($in "<a foo='bar'/>")
               (xml.read))
             '(a (:foo "bar"))))
  (assert (= (with-memory-stream ($in "<x> <v a='b' b='c'><y>yyy</y></v> <w d='e' e='f'><z>zzz</z></w> </x>")
               (xml.read))
             '(x ()
                 (v (:a "b" :b "c") (y () "yyy"))
                 (w (:d "e" :e "f") (z () "zzz")))))
  ;; Writer.
  (assert (= (with-memory-stream ($out)
               (xml.write '(a () "bar")))
             "<?xml version='1.0' encoding='UTF-8'?>\n<a>bar</a>\n"))
  (assert (= (with-memory-stream ($out)
               (xml.write-element '(a () "bar")))
             "<a>bar</a>\n"))
  (assert (= (with-memory-stream ($out)
               (xml.write-element
                 '(x ()
                     (v (:a "b" :b "c") (y () "yyy"))
                     (w (:d "e" :e "f") (z () "zzz")))))
             "<x><v a='b' b='c'><y>yyy</y></v><w d='e' e='f'><z>zzz</z></w></x>\n"))
  ;; SAX.
  (with-memory-stream ($in (str "<?xml version='1.0' encoding='UTF-8'?>\n"
                                "<img src='foo'/>"
                                "<!-- contents start -->"
                                "<div class='contents'>"
                                "    <ul class='none'>"
                                "        <li>foo</li>"
                                "        <li>bar</li>"
                                "    </ul>"
                                "</div>"))
    (let (elements nil classes nil texts (.new MemoryStream) comment nil)
      (.parse (.init (.new XML.SAXParser)
                     :on-read-text (f (text) (.write-bytes texts text))
                     :on-read-comment (f (text) (<- comment text))
                     :on-start-element (f (name attrs)
                                         (let (cls (cadr (member :class attrs)))
                                           (if cls (push! cls classes))))
                     :on-end-element (f (name) (push! name elements))))
      (assert (= (.to-s texts) "foobar"))
      (assert (= comment " contents start "))
      (assert (= (sort! (uniq elements)) (sort! '(div ul li))))
      (assert (= (sort! classes) (sort! '("contents" "none")))))))
