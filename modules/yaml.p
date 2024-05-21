; yaml module.

(import :json)

; In contrast to yaml, which places human readability as most important and ease of implementation as not a priority, the yaml module places ease of implementation as the highest priority and is designed for computers, not for people.
;
; The goal is to convert Paren data to yaml and to convert yaml to Paren data that is simple enough to be expressed in json.
;
; These functions are not supported.
;
; - Complex mapping key
; - Anchors and Aliases
; - Documents and Streams

(<- $yaml.indicator-chars'("-"    ; c-sequence-entry
                           "?"    ; c-mapping-key
                           ":"    ; c-mapping-value
                           ","    ; c-collect-entry
                           "["    ; c-sequence-start
                           "]"    ; c-sequence-end
                           "{"    ; c-mapping-start
                           "}"    ; c-mapping-end
                           "#"    ; c-comment
                           "&"    ; c-anchor
                           "*"    ; c-alias
                           "!"    ; c-tag
                           "|"    ; c-literal
                           ">"    ; c-folded
                           "'"    ; c-single-quote
                           "\""    ; c-double-quote
                           "%"    ; c-directive
                           "@" "`"))    ; c-reserved

(class YAML.Reader (AheadReader))

(method YAML.Reader .parse-string ()
  (.skip self)
  (while (!= (.next self) "\"") (.get-escape self))
  (.skip self)
  (.token self))

(method YAML.Reader .parse-scalar ()
  nil)

(method YAML.Reader .parse-block-scaler ()
  (let (indicator (.skip self) separator (if (= indicator "<") " " "\n") indent 0)
    (with-memory-stream ($out)
      (.skip self "\n")
      (while (= (.next self) " ")
        (.skip self)
        (<- indent (++ indent)))
      (write-line (.skip-line self))
      (while (= (.next self) " ")
        (dotimes (_ indent)
          (.skip self " "))
        (write-bytes separator)
        (write-bytes (.skip-line self)))
      (write-bytes "\n"))))

(method YAML.Reader .parse-block-sequece ()
  (let (lis nil)
    (while (= (.next (.skip-space self)) "-")
      (.skip self)
      (push! (.read self) lis))))

(method YAML.Reader .parse-flow-sequece ()
  nil)

(method YAML.Reader .parse-block-mapping (key)
  (let (d (dict) val nil)
    d))

(method YAML.Reader .parse-flow-mapping ()
  nil)

(method YAML.Reader .parse-identifier ()
  (while (.next? self (f (x) (! (in? x $yaml.indicator-chars))))
    (.get self))
  (.token self))

(method YAML.Reader .read ()
  (let (next (.next (.skip-space self)))
    (if (nil? next) nil
        (= next "#") (begin (.skip-line self) (.read self))
        (= next "-") (.parse-block-sequece self)
        (= next "[") (.parse-flow-sequece self)
        (= next "{") (.parse-flow-mapping self)
        (= next "\"") (.parse-string self)
        (in? next '("|" ">")) (.parse-block-scaler self)
        (= next "?") (raise NotImplementedError "complex mapping key is not supported")
        (= next "&") (raise NotImplementedError "node's anchor property is not supported")
        (= next "*") (raise NotImplementedError "alias node is not supported")
        (= next "!") (raise NotImplementedError "node tag is not supported")
        (in? next '("@" "`")) (raise SyntaxError "reserved indicator")
        (let (sym (.parse-identifier self) next (.next self))
          (if (= next ":") (.parse-block-mapping self sym)
              sym)))))    ; string.

(function yaml.read ()
  (.read (.new YAML.Reader)))

(function yaml.write (x)
  (json.write x))    ; YAML is a superset of JSON.

(function! main (args)
  ;; comment
  (assert (nil? (with-memory-stream ($in "# foo\n# bar") (yaml.read))))
  ;; scalar
  ;;; string
  (assert (= (with-memory-stream ($in "\"foo\"") (yaml.read)) "foo"))
  (assert (= (with-memory-stream ($in "foo") (yaml.read)) "foo"))
  ;;; literal block scalar
  (assert (= (with-memory-stream ($in "literal: |\n  some\n  text\n folded: >\n  some\n  text\n")
               ;; literal: |
               ;;   some
               ;;   text
               ;; folded: >
               ;;   some
               ;;   text
               (yaml.read))
             #{ literal "some\ntext\n" folded "some text\n" }))
  ;; sequence
  (assert (= (with-memory-stream ($in "- Mark McGwire\n- Sammy Sosa\n- Ken Griffey")
               (yaml.read))
             (list "Mark McGwire"
                   "Sammy Sosa"
                   "Ken Griffey"))))
