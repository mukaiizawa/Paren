; html module.

(import :xml)

(<- $dom.singleton '(area base br col command embed hr img input keygen link meta param source track wbr))

;; reader.

(class HTML.Reader (Object XML.Reader)
  stream)

(method HTML.Reader .skip-doctype ()
  (if (.match? self "<!") (while (!= (.skip self) ">"))))

(method HTML.Reader .read-attrs ()
  (let (ch nil attrs nil)
    (while (! (in? (<- ch (.next (.skip-space self))) '("/" ">")))
      (if (nil? ch) (raise EOFError "missing '>'")
          (push! (keyword (.read-ident self)) attrs))
      (.skip-space self)
      (if (= (.next self) "=") (.skip self)
          (continue))    ; Unlike xml, single attribute is allowed.
      (push! (.read-quoted (.skip-space self)) attrs))
    (reverse! attrs)))

(method HTML.Reader .read-element ()
  (if (nil? (.next self)) nil
      (.text-node? self) (.read-text self)
      (let ((type val) (.read-tag (.skip-space self)))
        (if (== type :comment) (.read-element self)
            (in? type '(:close :single)) val    ; make sense
            (let (name (car val) node nil children nil)
              (if (in? name $dom.singleton) val
                  (begin
                    (while (<- child (.read-element self))
                      (if (= name child) (break)
                          (symbol? child) (raise SyntaxError (str "unexpected close tag " child " expected " name))
                          (push! child children)))
                    (concat val (reverse! children)))))))))

(method HTML.Reader .read ()
  ; Read dom.
  (.skip-doctype self)
  (.read-element self))

;; API.

(function html.read ()
  (.read (.new HTML.Reader)))

(function html.write-element (x)
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
                   write1 (f (x)
                            (if (nil? x) nil
                                (string? x) (dostring (ch x)
                                              (write-bytes (if (= ch "\"") "&quot;"
                                                               (= ch "'") "&apos;"
                                                               (= ch "<") "&lt;"
                                                               (= ch ">") "&gt;"
                                                               (= ch "&") "&amp;"
                                                               ch)))
                                (cons? x) (let ((name attrs :rest children) x)
                                            (write-bytes "<") (write-bytes name) (write-attr attrs) (write-bytes ">")
                                            (when (! (in? name $dom.singleton))
                                              (foreach write1 children)
                                              (write-bytes "</") (write-bytes name) (write-bytes ">")))
                                (raise SyntaxError "unexpected expression"))))
    (write1 x)
    (write-line)
    x))

(function html.write (x)
  (write-line "<!DOCTYPE html>")
  (html.write-element x))

(function! main (args)
  ;; Reader.
  (assert (= (with-memory-stream ($in "<!DOCTYPE html>\n<html lang='ja'>hello html</html>")
               (html.read))
             '(html (:lang "ja") "hello html")))
  (assert (= (with-memory-stream ($in "<img src='./x.png'>")
               (html.read))
             '(img (:src "./x.png"))))
  ;; Writer.
  (assert (= (with-memory-stream ($out)
               (html.write '(html (:lang "ja") "hello html")))
             "<!DOCTYPE html>\n<html lang='ja'>hello html</html>\n"))
  (assert (= (with-memory-stream ($out)
               (html.write-element '(html (:lang "ja") "hello html")))
             "<html lang='ja'>hello html</html>\n"))
  (assert (= (with-memory-stream ($out)
               (html.write-element '(img (:src "./x.png"))))
             "<img src='./x.png'>\n")))
