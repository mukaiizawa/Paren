; set module.

(class Set ()
  eq? elements)

(method Set .init (eq?)
  (&eq?! self eq?))

(method Set .include? (val)
  (let (eq? (&eq? self))
    (dolist (elt (&elements self))
      (if (eq? elt val) (return true)))
    (return nil)))

(method Set .add (val)
  (if (! (.include? self val))
      (&elements! self (cons val (&elements self))))
  self)

(method Set .elements ()
  (&elements self))

(method Set .size ()
  (len (&elements self)))

(function! main (args)
  (let (set (.init (.new Set) =))
    (assert (! (.include? set "foo")))
    (.add set "foo")
    (assert (.include? set "foo"))
    (.add set "bar")
    (.add set "foo")
    (assert (= (.size set) 2))))
