; array module.

(class Array ()
  ; Extensible array class.
  size elt)

(method Array .init ()
  (<- self->size 0
      self->elt (array 4))
  self)

(method Array .size ()
  ; Returns size of the receiver.
  self->size)

(method Array .at (i)
  ; Returns i-th element of the receiver.
  ([] self->elt i))

(method Array .put (i val)
  ; Update the i-th element of the receiver to val.
  ; Returns the receiver.
  ([] self->elt i val)
  self)

(method Array .reserve (size)
  (let (req (+ self->size size) elt-size (len self->elt))
    (when (< elt-size req)
      (while (< (<- elt-size (* elt-size 2)) req))
      (let (src self->elt dst (array elt-size))
        (dotimes (i self->size) ([] dst i ([] src i)))
        (<- self->elt dst)))
    self))

(method Array .add (val)
  ; Add an element to the end of the array.
  ; Returns the receiver.
  (let (i self->size)
    (.reserve self 1)
    (<- self->size (++ i))
    ([] self->elt i val))
  self)

(method Array .to-a ()
  ; Returns an array representation of the receiver.
  (slice self->elt 0 self->size))

(function! main (args)
  (let (a (.new Array))
    (assert (= (.size a) 0))
    (.add a :foo)
    (assert (= (.at a 0) :foo))
    (assert (= (.at (.put a 0 :bar) 0) :bar))
    (assert (= (.to-a a) #[ :bar ]))))
