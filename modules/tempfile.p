; temporary file module.

(import :rand)

(function tempfile.name (prefix dot suffix)
  (str prefix (rand.str 32 :alnum? true) dot suffix))

(method Path .tempdir (:key prefix suffix)
  ; Create an temporary directory under the receiver.
  ; Returns the created directory.
  (let (p nil)
    (while (! (.none? (<- p (.resolve self (tempfile.name prefix nil suffix))))))
    (.mkdir p)))

(method Path .tempfile (:key prefix suffix)
  ; Create an temporary file under the receiver.
  ; Returns the created file.
  ; If suffix is omitted, it is considered that "wk" is specified.
  ; It is the caller's responsibility to delete the file.
  (let (p nil)
    (while (! (.none? (<- p (.resolve self (tempfile.name prefix "." (|| suffix "wk")))))))
    (.close (.open p :write))
    p))

(function! main (args)
  (let (dir (.tempdir (path ".")) temp (.tempfile dir))
    (assert (.dir? dir))
    (assert (.readable? temp))
    (with-open ($out temp :write) (println "foo"))
    (with-open ($in temp :read) (assert (= (read-line) "foo")))
    (.remove temp)
    (.remove dir)
    (assert (.none? dir))
    (assert (.none? temp))))
