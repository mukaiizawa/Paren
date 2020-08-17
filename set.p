; set module

(class Set ()
  cmp elements)

(method Set .init (:opt cmp)
  (&cmp<- self (|| cmp eq?)))

(method Set .include? (val)
  (let (cmp (&cmp self))
    (dolist (elt (&elements self))
      (if (cmp elt val) (return true)))
    (return nil)))

(method Set .add (val)
  (if (! (.include? self val))
      (&elements<- self (cons val (&elements self))))
  val)

(method Set .elements ()
  (&elements self))

(function! main (args)
  (let (set (.init (.new Set) string=))
    (assert (! (.include? set "foo")))
    (assert (string= (.add set "foo") "foo"))
    (assert (.include? set "foo"))
    (assert (string= (.add set "bar") "bar"))
    (assert (string= (.add set "foo") "foo"))
    (assert (= (length (.elements set)) 2))))
