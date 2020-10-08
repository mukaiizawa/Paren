; set module.

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
  (let (set (.init (.new Set) memeq?))
    (assert (! (.include? set "foo")))
    (assert (memeq? (.add set "foo") "foo"))
    (assert (.include? set "foo"))
    (assert (memeq? (.add set "bar") "bar"))
    (assert (memeq? (.add set "foo") "foo"))
    (assert (= (length (.elements set)) 2))))
