; json module.

(import :array)

(class JSONReader (AheadReader))

(method JSONReader .parse-object ()
  (let (object nil)
    (.skip self "{")
    (when (!= (.next (.skip-space self)) "}")
      (push! (keyword (.parse-string (.skip-space self))) object)
      (.skip (.skip-space self) ":")
      (push! (.read self) object))
    (while (!= (.next (.skip-space self)) "}")
      (.skip self ",")
      (push! (keyword (.parse-string (.skip-space self))) object)
      (.skip (.skip-space self) ":")
      (push! (.read self) object))
    (.skip self)
    (reverse! object)))

(method JSONReader .parse-array ()
  (let (a (.new Array))
    (.skip self "[")
    (when (!= (.next (.skip-space self)) "]")
      (.add a (.read self)))
    (while (!= (.next (.skip-space self)) "]")
      (.skip self ",")
      (.add a (.read self)))
    (.skip self)
    (.to-a a)))

(method JSONReader .parse-string ()
  (.skip self)
  (while (!= (.next self) "\"") (.get-escape self))
  (.skip self)
  (.token self))

(method JSONReader .parse-literal ()
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

(method JSONReader .read ()
  ; Read json. -- specified by RFC 8259.
  (let (next (.next (.skip-space self)))
    (if (nil? next) nil
        (= next "{") (.parse-object self)
        (= next "[") (.parse-array self)
        (= next "\"") (.parse-string self)
        (.parse-literal self))))

(function json->str (x)
  ; Returns a list representation of json as a string.
  (if (cons? x) (with-memory-stream ($out)
                  (write-bytes "{")
                  (let (i 0)
                    (while x
                      (if (> i 0) (write-bytes ",")
                          (<- i (++ i)))
                      (write-bytes (json->str (car x)))
                      (write-bytes ":")
                      (<- x (cdr x))
                      (assert x)    ; must be pair
                      (write-bytes (json->str (car x)))
                      (<- x (cdr x))))
                  (write-bytes "}"))
      (array? x) (with-memory-stream ($out)
                   (write-bytes "[")
                   (for (i 0) (< i (len x)) (i (++ i))
                     (if (> i 0) (write-bytes ","))
                     (write-bytes (json->str ([] x i))))
                   (write-bytes "]"))
      (nil? x) "null"
      (= x 'true) "true"
      (= x 'false) "false"
      (number? x) (str x)
      (str "\"" x "\"")))

(function! main (args)
  (let (json-str (str "{"
                      "  \"nodes\": ["
                      "     {\"id\":49,\"name\":\"object_p\", \"time\":0.73},"
                      "     {\"id\":21,\"name\":\"object_list_len\", \"time\":3.58}"
                      "  ],"
                      "  \"literal\": [true, false, null, 3.14, \"string\"]"
                      "}"))
    (with-memory-stream ($in json-str)
      (let (json (.read (.new JSONReader))
                 (nodes-key nodes-val literal-key literal-val) json)
        (assert (= nodes-key :nodes))
        (assert (array? nodes-val))
        (let (nodes0 ([] nodes-val 0) nodes1 ([] nodes-val 1))
          (assert (= (car nodes0) :id))
          (assert (= (cadr nodes0) 49))
          (assert (= (caddr nodes0) :name))
          (assert (= (cadddr nodes0) "object_p")))
        (assert (= ([] literal-val 0) true))
        (assert (= ([] literal-val 1) 'false))
        (assert (= ([] literal-val 2) nil))
        (assert (= ([] literal-val 3) 3.14))
        (assert (= ([] literal-val 4) "string"))
        (assert (= (json->str json)
                   (join '("{\"nodes\":["
                           "{\"id\":49,\"name\":\"object_p\",\"time\":0.73},"
                           "{\"id\":21,\"name\":\"object_list_len\",\"time\":3.58}"
                           "],\"literal\":[true,false,null,3.14,\"string\"]}"))))))))
