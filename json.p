; json module.

(function json-object->string (lis)
  (with-memory-stream ($out)
    (write-bytes "{")
    (let (i 0)
      (while lis
        (if (> i 0) (write-bytes ",")
            (<- i (++ i)))
        (write-bytes (json->string (car lis)))
        (write-bytes ":")
        (<- lis (cdr lis))
        (assert lis)    ; must be pair
        (write-bytes (json->string (car lis)))
        (<- lis (cdr lis))))
    (write-bytes "}")))

(function json-array->string (arr)
  (with-memory-stream ($out)
    (write-bytes "[")
    (for (i 0) (< i (array-length arr)) (<- i (++ i))
      (if (> i 0) (write-bytes ","))
      (write-bytes (json->string ([] arr i))))
    (write-bytes "]")))

(function json->string (x)
  ; Returns a list representation of json as a string.
  (if (cons? x) (json-object->string x)
      (array? x) (json-array->string x)
      (nil? x) "null"
      (eq? x 'true) "true"
      (eq? x 'false) "false"
      (number? x) (string x)
      (string "\"" (bytes->string x) "\"")))

(class JSONReader (AheadReader))

(method JSONReader .parse-object ()
  (let (object nil)
    (.skip self "{")
    (when (string/= (.next (.skip-space self)) "}")
      (push! object (bytes->keyword (.parse-string (.skip-space self))))
      (.skip (.skip-space self) ":")
      (push! object (.read self)))
    (while (string/= (.next (.skip-space self)) "}")
      (.skip self ",")
      (push! object (bytes->keyword (.parse-string (.skip-space self))))
      (.skip (.skip-space self) ":")
      (push! object (.read self)))
    (.skip self)
    (reverse! object)))

(method JSONReader .parse-array ()
  (let (a (.new Array))
    (.skip self "[")
    (when (string/= (.next (.skip-space self)) "]")
      (.add a (.read self)))
    (while (string/= (.next (.skip-space self)) "]")
      (.skip self ",")
      (.add a (.read self)))
    (.skip self)
    (.to-a a)))

(method JSONReader .parse-string ()
  (.skip self)
  (while (string/= (&next self) "\"") (.get-escape self))
  (.skip self)
  (.token self))

(method JSONReader .parse-literal ()
  (if (.digit? self) (.skip-number self)
      (string= (.next self) "t") (begin
                                   (dostring (c "true") (.skip self c))
                                   'true)
      (string= (.next self) "f") (begin
                                   (dostring (c "false") (.skip self c))
                                   'false)
      (string= (.next self) "n") (begin
                                   (dostring (c "null") (.skip self c))
                                   nil)
      (.raise self "unexpected token")))

(method JSONReader .read ()
  ; Read json. -- specified by RFC 8259.
  (let (next (.next (.skip-space self)))
    (if (nil? next) nil
        (string= next "{") (.parse-object self)
        (string= next "[") (.parse-array self)
        (string= next "\"") (.parse-string self)
        (.parse-literal self))))

(function! main (args)
  (with-memory-stream
    ($in "{
        \"nodes\": [
                    {\"id\":49,\"name\":\"object_p\", \"time\":0.73},
                    {\"id\":36,\"name\":\"object_list_p\", \"time\":1.58},
                    {\"id\":21,\"name\":\"object_list_len\", \"time\":3.58}
                    ],
        \"links\": [
                    {\"source\":100,\"target\":100},
                    {\"source\":100,\"target\":100},
                    {\"source\":99,\"target\":99},
                    {\"source\":99,\"target\":99},
                    {\"source\":98,\"target\":49}
                    ],
        \"literal\": [true, false, null, 3.14, \"string\"]
        }")
        (write (json->string (write (.read (.init (.new JSONReader))))))))
