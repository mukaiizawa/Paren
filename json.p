; json module.

(function json.obj->str (lis)
  (with-memory-stream ($out)
    (write-mem "{")
    (let (i 0)
      (while lis
        (if (> i 0) (write-mem ",")
            (<- i (++ i)))
        (write-mem (json->str (car lis)))
        (write-mem ":")
        (<- lis (cdr lis))
        (assert lis)    ; must be pair
        (write-mem (json->str (car lis)))
        (<- lis (cdr lis))))
    (write-mem "}")))

(function json.arr->str (arr)
  (with-memory-stream ($out)
    (write-mem "[")
    (for (i 0) (< i (arrlen arr)) (<- i (++ i))
      (if (> i 0) (write-mem ","))
      (write-mem (json->str ([] arr i))))
    (write-mem "]")))

(function json->str (x)
  ; Returns a list representation of json as a string.
  (if (cons? x) (json.obj->str x)
      (array? x) (json.arr->str x)
      (nil? x) "null"
      (eq? x 'true) "true"
      (eq? x 'false) "false"
      (number? x) (string x)
      (string "\"" (mem->str x) "\"")))

(class JSONReader (AheadReader))

(method JSONReader .parse-object ()
  (let (object nil)
    (.skip self "{")
    (when (memneq? (.next (.skip-space self)) "}")
      (push! object (mem->key (.parse-string (.skip-space self))))
      (.skip (.skip-space self) ":")
      (push! object (.read self)))
    (while (memneq? (.next (.skip-space self)) "}")
      (.skip self ",")
      (push! object (mem->key (.parse-string (.skip-space self))))
      (.skip (.skip-space self) ":")
      (push! object (.read self)))
    (.skip self)
    (reverse! object)))

(method JSONReader .parse-array ()
  (let (a (.new Array))
    (.skip self "[")
    (when (memneq? (.next (.skip-space self)) "]")
      (.add a (.read self)))
    (while (memneq? (.next (.skip-space self)) "]")
      (.skip self ",")
      (.add a (.read self)))
    (.skip self)
    (.to-a a)))

(method JSONReader .parse-string ()
  (.skip self)
  (while (memneq? (&next self) "\"") (.get-escape self))
  (.skip self)
  (.token self))

(method JSONReader .parse-literal ()
  (if (.digit? self) (.skip-number self)
      (memeq? (.next self) "t") (begin
                                   (dostring (c "true") (.skip self c))
                                   'true)
      (memeq? (.next self) "f") (begin
                                   (dostring (c "false") (.skip self c))
                                   'false)
      (memeq? (.next self) "n") (begin
                                   (dostring (c "null") (.skip self c))
                                   nil)
      (.raise self "unexpected token")))

(method JSONReader .read ()
  ; Read json. -- specified by RFC 8259.
  (let (next (.next (.skip-space self)))
    (if (nil? next) nil
        (memeq? next "{") (.parse-object self)
        (memeq? next "[") (.parse-array self)
        (memeq? next "\"") (.parse-string self)
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
         (write (json->str (write (.read (.new JSONReader)))))))
