; json module.

(import :array)

(class JSON.Reader (AheadReader))

(method JSON.Reader .parse-object ()
  (let (object nil)
    (.skip self "{")
    (when (!= (.next (.skip-space self)) "}")
      (push! (symbol (.parse-string (.skip-space self))) object)
      (.skip (.skip-space self) ":")
      (push! (.read self) object))
    (while (!= (.next (.skip-space self)) "}")
      (.skip self ",")
      (push! (symbol (.parse-string (.skip-space self))) object)
      (.skip (.skip-space self) ":")
      (push! (.read self) object))
    (.skip self)
    (reverse! object)))

(method JSON.Reader .parse-array ()
  (let (a (.new Array))
    (.skip self "[")
    (when (!= (.next (.skip-space self)) "]")
      (.add a (.read self)))
    (while (!= (.next (.skip-space self)) "]")
      (.skip self ",")
      (.add a (.read self)))
    (.skip self)
    (.to-a a)))

(method JSON.Reader .parse-string ()
  (.skip self)
  (while (!= (.next self) "\"") (.get-escape self))
  (.skip self)
  (.token self))

(method JSON.Reader .parse-literal ()
  (if (.next? self digit?) (.skip-number self)
      (= (.next self) "t") (begin
                             (dostring (c "true") (.skip self c))
                             'true)
      (= (.next self) "f") (begin
                             (dostring (c "false") (.skip self c))
                             'false)
      (= (.next self) "n") (begin
                             (dostring (c "null") (.skip self c))
                             nil)
      (raise SyntaxError "unexpected token")))

(method JSON.Reader .read ()
  ; Read json. -- specified by RFC 8259.
  (let (next (.next (.skip-space self)))
    (if (nil? next) nil
        (= next "{") (.parse-object self)
        (= next "[") (.parse-array self)
        (= next "\"") (.parse-string self)
        (.parse-literal self))))

(function json.read ()
  ; Read json.
  ; Returns the list corresponding to json.
  (.read (.new JSON.Reader)))

(function json.write (x)
  ; Output json corresponding to the argument.
  ; Returns the argument.
  (if (cons? x) (let (i 0 pairs (group x 2))
                  (dolist (pair pairs)
                    (write-bytes (if (= i 0) "{" ","))
                    (printf "\"%v\":" (car pair))
                    (json.write (cadr pair))
                    (<- i (++ i)))
                  (write-bytes "}"))
      (array? x) (begin
                   (for (i 0) (< i (len x)) (i (++ i))
                     (write-bytes (if (= i 0) "[" ","))
                     (json.write ([] x i)))
                   (write-bytes "]"))
      (nil? x) (write-bytes "null")
      (== x 'true) (write-bytes "true")
      (== x 'false) (write-bytes "false")
      (write x :end "")))

(function! main (args)
  (let (json '(nodes #[ (id 49 name "object_p" time 0.73) (id 49 name "object_p" time 0.73) ] literal #[ true false nil 3.14 "string" ] )
             json-str "{\"nodes\":[{\"id\":49,\"name\":\"object_p\",\"time\":0.73},{\"id\":49,\"name\":\"object_p\",\"time\":0.73}],\"literal\":[true,false,null,3.14,\"string\"]}")
    (assert (= (with-memory-stream ($out) (json.write json)) json-str))
    (assert (= (with-memory-stream ($in json-str) (json.read)) json))))
