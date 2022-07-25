; DOM module.

(<- $dom.singleton '(area base br col command embed hr img input keygen link meta param source track wbr))

;; reader.

(class DOM.Reader ()
  stream)

(method DOM.Reader .init ()
  (<- self->stream (.new MemoryStream))
  (.write-bytes self->stream (read-bytes))
  self)

(method DOM.Reader .skip (:opt size)
  (dotimes (i (|| size 1))
    (.read-char self->stream))
  self)

(method DOM.Reader .match? (text)
  (let (match? true pos (.tell self->stream))
    (dostring (ch text)
      (when (!= ch (.read-char self->stream))
        (<- match? nil)
        (break)))
    (.seek self->stream pos)
    match?))

(method DOM.Reader .skip-space ()
  (let (ch nil)
    (while (<- ch (.peek-char self->stream))
      (if (space? ch) (.skip self)
          (break)))
    self))

(method DOM.Reader .skip-doctype ()
  (while (!= (.read-char self->stream) "<"))
  (if (!= (.read-char self->stream) "!") (raise SyntaxError "missing DOCTYPE")
  (while (!= (.read-char self->stream) ">"))))

(method DOM.Reader .read-quoted ()
  (with-memory-stream (val)
    (let (ch nil quote (.read-char self->stream))
      (if (! (in? quote '("'" "\""))) (raise SyntaxError "expected quote")
          (while (!= (<- ch (.read-char self->stream)) quote)
            (if (nil? ch) (raise SyntaxError "missing quote")
                (.write-bytes val ch)))))))

(method DOM.Reader .read-ident ()
  (let (ch nil ident (.new MemoryStream))
    (while (<- ch (.peek-char self->stream))
      (if (nil? ch) (raise SyntaxError "expected '>'")
          (alnum? ch) (.write-bytes ident (.read-char self->stream))
          (break)))
    (symbol (.to-s ident))))

(method DOM.Reader .read-text ()
  (with-memory-stream (text)
    (let (ch nil)
      (while (<- ch (.peek-char self->stream))
        (if (= ch "<") (break)
            (!= ch "&") (.write-bytes text (.read-char self->stream))
            (begin
              (.read-char self->stream)
              (.write-bytes text
                            (if (.match? self "&quot;") "\""
                                (.match? self "&apos;") "'"
                                (.match? self "&lt;") "<"
                                (.match? self "&gt;") ">"
                                (.match? self "&amp;") "&"
                                "&"))))))))

(method DOM.Reader .read-attrs ()
  (let (ch nil attrs nil)
    (while (!= (<- ch (.peek-char self->stream)) ">")
      (if (nil? ch) (raise EOFError "missing '>'")
          (push! (keyword (.read-ident self)) attrs))
      (.skip-space self)
      (if (= (.peek-char self->stream) "=") (.skip self)
          (continue))    ; single attribute
      (push! (.read-quoted (.skip-space self)) attrs))
    (reverse! attrs)))

(method DOM.Reader .skip-comment ()
  (.skip self 3)
  (while (! (.match? self "-->"))
    (.read-char self->stream))
  (.skip self 3))

(method DOM.Reader .read-close-tag ()
  (.skip self)
  (let (name (.read-ident self))
    (if (!= (.read-char self->stream) ">") (raise SyntaxError "expected end tag '>'")
        (list nil name))))

(method DOM.Reader .read-tag ()
  (.skip self)
  (if (= (.peek-char self->stream) "!") (begin (.skip-comment self) (.read-tag self))
      (= (.peek-char self->stream) "/") (.read-close-tag self)    ; end-element
      (let (name (.read-ident self) attrs (.read-attrs self))
        (.skip-space self)
        (if (!= (.read-char self->stream) ">") (raise SyntaxError "expected close tag '>'")
            (list true (list name attrs))))))

(method DOM.Reader .read-node ()
  (if (!= (.peek-char self->stream) "<") (.read-text self)
      (let ((trail? tag) (.read-tag self))
        (if (! trail?) tag    ; make sense
            (let (name (car tag) child nil children nil)
              (if (in? name $dom.singleton) tag
                  (begin
                    (while (!= name (<- child (.read-node self)))
                      (if (symbol? child) (raise SyntaxError (str "unexpected close tag " child " expected " name))
                          (push! child children)))
                    (concat tag (reverse! children)))))))))

(method DOM.Reader .read ()
  ; Read dom.
  (.skip-doctype self)
  (.read-node (.skip-space self)))

;; query-selector.

(class DOM.SelectorCompiler ()
  reader)

(method DOM.SelectorCompiler .init ()
  (<- self->reader (.new AheadReader))
  self)

(method DOM.SelectorCompiler .parse-selector0 ()
  (let (selectors nil next nil
                  lead-basic-selector? (f (rd) (in? (.next rd) '("#" "." "[")))
                  read-ident (f (rd)
                               (if (= (.next rd) "*") (begin (.skip rd) "*")
                                   (begin
                                     (while (.next? rd alnum?)
                                       (.get rd))
                                     (.token rd)))))
    (if (! (lead-basic-selector? self->reader)) (push! `(:name ,(read-ident self->reader)) selectors))
    (while (lead-basic-selector? self->reader)
      (<- next (.skip self->reader))
      (push!
        (if (= next "#") `(:id ,(read-ident self->reader))
            (= next ".") `(:class ,(read-ident self->reader))
            (= next "[") (let (name (read-ident self->reader) next (.skip self->reader)
                                    read-val (f (rd)
                                               (let (ch (.skip rd))
                                                 (if (! (in? ch '("'" "\""))) (raise Error "missing attribute value quote")
                                                     (begin
                                                       (while (!= (.next rd) ch)
                                                         (.get rd))
                                                       (.skip rd ch)
                                                       (.skip self->reader "]")
                                                       (.token rd))))))
                           (if (= next "]") `(:attr ,name)
                               (= next "=") `(:attr= ,(list name (read-val self->reader)))
                               (in? next '("~" "|" "^" "$" "*")) (let (ope (keyword (str "attr" next (.skip self->reader "="))))
                                                                   `(,ope ,(list name (read-val self->reader))))
                               (raise Error "unexpected attribute selector"))))
        selectors))
    (apply concat (reverse! selectors))))

(method DOM.SelectorCompiler .parse-selector ()
  (let (selector (list (.parse-selector0 self)) next nil)
    (while (<- next (.next (.skip-space self->reader)))
      (if (= next ",") (break)
          (= next ">") (begin (.skip self->reader) (push! :child-combinator selector))
          (= next "~") (begin (.skip self->reader) (push! :general-sibling-combinator selector))
          (= next "+") (begin (.skip self->reader) (push! :adjacent-sibling-combinator selector))
          (push! :descendant-combinator selector))
      (.skip-space self->reader)
      (push! (.parse-selector0 self) selector))
    selector))

(method DOM.SelectorCompiler .compile ()
  (let (selectors nil)
    (push! (.parse-selector self) selectors)
    (while (= (.next (.skip-space self->reader)) ",")
      (.skip self->reader)
      (.skip-space self->reader)
      (push! (.parse-selector self) selectors))
    (reverse! selectors)))

(function dom.compile-selector (selector)
  (if (! (string? selector)) selector
      (with-memory-stream ($in selector)
        (.compile (.new DOM.SelectorCompiler)))))

;; API.

(function dom.name (node)
  (lower (string (car node))))

(function dom.attributes (node)
  (keep (f (x) (if (keyword? x) (string x)))
        (cadr node)))

(function dom.attribute (node attr)
  (cadr (member (keyword attr) (cadr node))))

(function dom.id (node)
  (dom.attribute node :id))

(function dom.classes (node)
  (split (dom.attribute node :class) " "))

(function dom.children (node)
  (cddr node))

(function dom.first-child (node)
  (car (dom.children node)))

(function dom.text-node? (node)
  (string? node))

(function dom.get-element-by-id (dom id)
  (dom.query-selector dom (str "#" id)))

(function dom.get-element-by-name (dom name)
  (dom.query-selector dom name))

(function dom.query-selector (dom selector)
  (car (dom.query-selector-all dom selector)))

(function dom.query-selector-all (dom selector)
  (let (nodes nil selectors (dom.compile-selector selector)
              nth-child (f (node i) (&& (>= i 0) ([] (dom.children node) i)))
              basic-match? (f (node expr)
                             (every? (f (x)
                                       (let ((type args) x)
                                         (if (== type :name) (|| (= args "*")
                                                                 (= args (dom.name node)))
                                             (== type :id) (= args (dom.id node))
                                             (== type :class) (in? args (dom.classes node))
                                             (== type :attr) (in? args (dom.attributes node))
                                             (== type :attr=) (= (dom.attribute node (car args)) (cadr args))
                                             (== type :attr^=) (prefix? (dom.attribute node (car args)) (cadr args))
                                             (== type :attr$=) (suffix? (dom.attribute node (car args)) (cadr args))
                                             (raise NotImplementedError))))
                                     (group expr 2)))
              match? (f (exprs node parents indices)
                       (if (nil? node) nil
                           (basic-match? node (pop! exprs))
                           (let (combinator (pop! exprs))
                             (if (nil? combinator) true    ; match!
                                 (== combinator :descendant-combinator) (while parents
                                                                          (if (match? exprs (pop! parents) parents (cdr indices)) (return true)))
                                 (== combinator :child-combinator) (match? exprs (pop! parents) parents (cdr indices))
                                 (== combinator :general-sibling-combinator) (dotimes (i (pop! indices))
                                                                               (if (match? exprs (nth-child (car parents) i) parents (cons i indices)) (return true)))
                                 (== combinator :adjacent-sibling-combinator) (let (i (-- (pop! indices)))
                                                                                (match? exprs (nth-child (car parents) i) parents (cons i indices)))
                                 (raise NotImplementedError)))))
              sweep (f (exprs node parents indices)
                      (if (match? exprs node parents indices) (push! node nodes))
                      (let (parents (cons node parents) indice 0)
                        (dolist (child (dom.children node))
                          (if (! (dom.text-node? child)) (sweep exprs child parents (cons indice indices)))
                          (<- indice (++ indice))))))
    (dolist (selector selectors)
      (sweep selector dom nil nil))
    (reverse! nodes)))

(function dom.read ()
  (.read (.new DOM.Reader)))

(function dom.write (x)
  (let (write-attr (f (x)
                     (if (nil? x) nil
                         (! (cons? x)) (raise SyntaxError "attributes must be list")
                         (let (rest x curr (car x))
                           (while rest
                             (if (! (keyword? curr)) (raise SyntaxError "attribute name must be keyword")
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
                                       (raise SyntaxError "attribute value must be string"))))))))
                   write-node (f (x)
                                (if (string? x) (dostring (ch x)
                                                  (write-bytes (if (= ch "\"") "&quot;"
                                                                   (= ch "'") "&apos;"
                                                                   (= ch "<") "&lt;"
                                                                   (= ch ">") "&gt;"
                                                                   (= ch "&") "&amp;"
                                                                   ch)))
                                    (cons? x) (let ((name attrs :rest children) x)
                                                (write-bytes "<") (write-bytes name) (write-attr attrs) (write-bytes ">")
                                                (when (! (in? name $dom.singleton))
                                                  (foreach write-node children)
                                                  (write-bytes "<") (write-bytes name) (write-bytes ">")))
                                    (raise SyntaxError "unexpected expression"))))
    (write-line "<!DOCTYPE html>")
    (write-node x)
    (write-line)
    x))

(function! main (args)
  ;; reader.
  (with-memory-stream ($in "<!DOCTYPE html>\n<html>hello html</html>")
    (assert (= (dom.read)
               '(html () "hello html"))))
  ;; accessor.
  (let (dom '(div (:id "container" :class "v")
                  (p (:class "x y z") "text")
                  (input (:type "text"))))
    (assert (= (dom.id dom) "container"))
    (assert (= (dom.name dom) "div"))
    (assert (= (dom.attributes dom) '("id" "class")))
    (assert (= (dom.classes (car (dom.children dom))) '("x" "y" "z")))
    (assert (= (dom.attribute (cadr (dom.children dom)) "type") "text")))
  ;; compiler.
  (assert (= (dom.compile-selector "input")
             '(((:name "input")))))
  (assert (= (dom.compile-selector "#id")
             '(((:id "id")))))
  (assert (= (dom.compile-selector ".small")
             '(((:class "small")))))
  (assert (= (dom.compile-selector "[title]")
             '(((:attr "title")))))
  (assert (= (dom.compile-selector "[title='foo']")
             '(((:attr= ("title" "foo"))))))
  (assert (= (dom.compile-selector "[title|='foo']")
             '(((:attr|= ("title" "foo"))))))
  (assert (= (dom.compile-selector "input.small")
             '(((:name "input" :class "small")))))
  (assert (= (dom.compile-selector ".red.blue")
             '(((:class "red" :class "blue")))))
  (assert (= (dom.compile-selector "img.xxx[src^='https']")
             '(((:name "img" :class "xxx" :attr^= ("src" "https"))))))
  (assert (= (dom.compile-selector "div p")
             '(((:name "p") :descendant-combinator (:name "div")))))
  (assert (= (dom.compile-selector "ul > li")
             '(((:name "li") :child-combinator (:name "ul")))))
  (assert (= (dom.compile-selector "img + p")
             '(((:name "p") :adjacent-sibling-combinator (:name "img")))))
  (assert (= (dom.compile-selector "img ~ p")
             '(((:name "p") :general-sibling-combinator (:name "img")))))
  (assert (= (dom.compile-selector "a.a #id > .cls")
             '(((:class "cls") :child-combinator (:id "id") :descendant-combinator (:name "a" :class "a")))))
  (assert (= (dom.compile-selector "a, .b, #c, [d='e']")
             '(((:name "a"))
               ((:class "b"))
               ((:id "c"))
               ((:attr= ("d" "e"))))))
  (assert (= (dom.compile-selector "ol > li[class$='xxx'], ul > li.yyy")
             '(((:name "li" :attr$= ("class" "xxx")) :child-combinator (:name "ol"))
               ((:name "li" :class "yyy") :child-combinator (:name "ul")))))
  (assert (= (dom.compile-selector "a.cls, b > c.cls")
             '( ((:name "a" :class "cls"))
               ((:name "c" :class "cls") :child-combinator (:name "b")))))
  ;; query-selector.
  (assert (= (dom.query-selector-all '(div () (a ()) (a ()) (a ())) "a")
             '((a ()) (a ()) (a ()))))
  (assert (= (dom.query-selector-all '(a () (a () (a ()))) "a")
             '((a () (a () (a ())))
               (a () (a ()))
               (a ()))))
  (assert (= (dom.query-selector-all '(a () (a () (a ()))) "a a")
             '((a () (a ()))
               (a ()))))
  (assert (= (dom.query-selector-all '(a () (a () (a ()))) "a a a")
             '((a ()))))
  (assert (= (dom.query-selector-all '(div ()
                                           (p ())
                                           (p ())
                                           (span ())
                                           (p ()))
                                     "p + p")
             '((p ()))))
  (assert (= (dom.query-selector-all '(ul ()
                                          (li ())
                                          (li ())
                                          (li ()))
                                     "li ~ li")
             '((li ()) (li ()))))
  (assert (= (dom.query-selector-all '(ul ()
                                          (li (:id "li0"))
                                          (li (:id "li1"))
                                          (li (:id "li2")))
                                     "ul > li#li1 + li[id='li2']")
             '((li (:id "li2"))))))
