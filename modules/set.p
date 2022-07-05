; set module.

(class Set ()
  eq? elements)

(method Set .init (eq?)
  (<- self->eq? eq?)
  self)

(method Set .include? (val)
  (let (eq? self->eq?)
    (dolist (elt self->elements)
      (if (eq? elt val) (return true)))
    (return nil)))

(method Set .add (val)
  (if (! (.include? self val))
      (<- self->elements (cons val self->elements)))
  self)

(method Set .elements ()
  self->elements)

(method Set .size ()
  (len self->elements))

(function! main (args)
  (let (set (.init (.new Set) =))
    (assert (! (.include? set "foo")))
    (.add set "foo")
    (assert (.include? set "foo"))
    (.add set "bar")
    (.add set "foo")
    (assert (= (.size set) 2))))
