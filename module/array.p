; array module.

(class Array ()
  ; Extensible array class.
  size elt)

(method Array .init ()
  (&size! self 0)
  (&elt! self (array 4)))

(method Array .size ()
  ; Returns size of the receiver.
  (&size self))

(method Array .at (i)
  ; Returns i-th element of the receiver.
  ([] (&elt self) i))

(method Array .put (i val)
  ; Update the i-th element of the receiver to val.
  ; Returns the receiver.
  ([] (&elt self) i val)
  self)

(method Array .reserve (size)
  (let (req (+ (&size self) size) elt-size (len (&elt self)))
    (when (< elt-size req)
      (while (< (<- elt-size (* elt-size 2)) req))
      (let (src (&elt self) dst (array elt-size))
        (dotimes (i (&size self)) ([] dst i ([] src i)))
        (&elt! self dst)))
    self))

(method Array .add (val)
  ; Add an element to the end of the array.
  ; Returns the receiver.
  (let (i (&size self))
    (.reserve self 1)
    (&size! self (++ i))
    ([] (&elt self) i val))
  self)

(method Array .to-a ()
  ; Returns an array representation of the receiver.
  (slice (&elt self) 0 (&size self)))

(function! main (args)
  (let (a (.new Array))
    (assert (= (.size a) 0))
    (.add a :foo)
    (assert (= (.at a 0) :foo))
    (assert (= (.at (.put a 0 :bar) 0) :bar))
    (assert (= (.to-a a) #[ :bar ]))))
