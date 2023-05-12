; json module.

(import :array)

(class JSON.Reader (AheadReader))

(method JSON.Reader .parse-object-prop ()
  (let (key (symbol (.parse-string (.skip-space self))))
    (.skip (.skip-space self) ":")
    (list key (.read self))))

(method JSON.Reader .parse-object ()
  (let (object (dict))
    (.skip self "{")
    (when (!= (.next (.skip-space self)) "}")
      (apply [] (cons object (.parse-object-prop self)))
      (while (!= (.next (.skip-space self)) "}")
        (.skip self ",")
        (apply [] (cons object (.parse-object-prop self)))))
    (.skip self "}")
    object))

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
  (if (dict? x) (let (i 0)
                  (write-bytes "{")
                  (dolist (key (keys x))
                    (if (> i 0) (write-bytes ","))
                    (if (! (symbol? key)) (raise ArgumentError "invalid object property: %v" key)
                        (json.write (string key)))
                    (write-bytes ":")
                    (json.write ([] x key))
                    (<- i (++ i)))
                  (write-bytes "}"))
      (array? x) (begin
                   (write-bytes "[")
                   (for (i 0) (< i (len x)) (i (++ i))
                     (if (> i 0) (write-bytes ","))
                     (json.write ([] x i)))
                   (write-bytes "]"))
      (nil? x) (write-bytes "null")
      (== x 'true) (write-bytes "true")
      (== x 'false) (write-bytes "false")
      (|| (symbol? x) (number? x) (string? x)) (write x :end "")
      (raise ArgumentError "unexpected value: %v" x)))

(function! main (args)
  (let (json #{
                 nodes #[
                     #{ id 1 name "foo" time 0.1 }
                     #{ id 2 name "bar" time 0.22 }
                 ]
                 literal #[ true 'false nil 3.14 "string" ]
             })
    (assert (= (with-memory-stream ($in (with-memory-stream ($out) (json.write json))) (json.read)) json))))
