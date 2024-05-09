; yaml module.

(import :json)

; In contrast to yaml, which places human readability as most important and ease of implementation as not a priority, the yaml module places ease of implementation as the highest priority and is designed for computers, not for people.
;
; The most important goal is to output the data from the Paren as yaml, and reading the data from yaml as Paren data is a secondary goal.
;
; These functions are not supported.
;
; - Complex mapping key
; - Anchors and Aliases
; - Documents and Streams

(class YAML.Reader (AheadReader))

(method YAML.Reader .skip-indent (n)
  (dotimes (_ (* 2 n))
    (.skip self " ")))

(method YAML.Reader .parse-string ()
  (.skip self)
  (while (!= (.next self) "\"") (.get-escape self))
  (.skip self)
  (.token self))

(method YAML.Reader .parse-scalar ()
  nil)

(method YAML.Reader .parse-token ()
  nil)

  (method YAML.Reader .parse-block-sequece ()
    (let (lis nil)
      (while (= (.next (.skip-space self)) "-")
        (.skip self)
        (push! (.read self) lis))))

(method YAML.Reader .parse-flow-sequece ()
  nil)

(method YAML.Reader .parse-block-mapping ()
  nil)

(method YAML.Reader .parse-flow-mapping ()
  nil)

(method YAML.Reader .read ()
  (let (next (.next (.skip-space self)))
    (if (nil? next) nil
        (= next "#") (begin (.skip-line self) (.read self))
        (= next "-") (.parse-block-sequece self)
        (= next "[") (.parse-flow-sequece self)
        (= next "{") (.parse-flow-mapping self)
        (= next "\"") (.parse-string self)
        (= next "?") (raise SyntaxError "complex mapping key is not supported")
        (.parse-token self))))

(function yaml.read ()
  (.read (.new YAML.Reader)))

(function yaml.write (x)
  (json.write x))    ; YAML is a superset of JSON.

(function! main (args)
  ;; comment
  (assert (nil? (with-memory-stream ($in "
                                         # foo
                                         # bar
                                         ")
                                         (yaml.read))))
  ;; scalar
  ;;; string
  (assert (= (with-memory-stream ($in "\"foo\"") (yaml.read)) "foo"))
  (assert (= (with-memory-stream ($in "foo") (yaml.read)) "foo"))
  ;; sequence
  (assert (= (with-memory-stream ($in "
                                      - Mark McGwire
                                      - Sammy Sosa
                                      - Ken Griffey
                                      ")
                                      (yaml.read))
             (list "Mark McGwire"
                   "Sammy Sosa"
                   "Ken Griffey"))))
