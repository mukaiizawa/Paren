; set module.

(class Set ()
  cmp elements)

(method Set .init (cmp)
  (&cmp! self cmp))

(method Set .include? (val)
  (let (cmp (&cmp self))
    (dolist (elt (&elements self))
      (if (cmp elt val) (return true)))
    (return nil)))

(method Set .add (val)
  (if (! (.include? self val))
      (&elements! self (cons val (&elements self))))
  self)

(method Set .elements ()
  (&elements self))

(method Set .size ()
  (length (&elements self)))

(function! main (args)
  (let (set (.init (.new Set) memeq?))
    (assert (! (.include? set "foo")))
    (.add set "foo")
    (assert (.include? set "foo"))
    (.add set "bar")
    (.add set "foo")
    (assert (= (.size set) 2))))
