; ring bufer module.

(class Ring ()
  buf rdpos)

(method Ring .init (size)
  (<- self->buf (array (++ size))
      self->rdpos 0
      self->wrpos 0)
  self)

(method Ring .next-pos (pos)
  (% (++ pos) (len self->buf)))

(method Ring .get ()
  (if (= self->wrpos self->rdpos) (raise StateError)
      (let (rdpos self->rdpos)
        (<- self->rdpos (.next-pos self rdpos))
        ([] self->buf rdpos))))

(method Ring .put (x)
  (let (wrpos self->wrpos)
    ([] self->buf wrpos x)
    (<- self->wrpos (.next-pos self wrpos))
    (if (= self->wrpos self->rdpos) (raise StateError)
        self)))

(method Ring .size ()
  (let (wrpos self->wrpos rdpos self->rdpos)
    (if (<= rdpos wrpos) (- wrpos rdpos)
        (+ (- (len self->buf) rdpos) wrpos))))

(function! main (args)
  (let (ring (.init (.new Ring) 3))
    (assert (= (.size ring) 0))
    (assert (= (.size (.put ring :a)) 1))
    (assert (= (.get ring) :a))
    (assert (= (.size ring) 0))
    (assert (= (.size (.put ring :a)) 1))
    (assert (= (.size (.put ring :b)) 2))
    (assert (= (.size (.put ring :c)) 3))
    (assert (= (.get ring) :a))
    (assert (= (.size ring) 2))
    (assert (= (.get ring) :b))
    (assert (= (.size ring) 1))
    (assert (= (.get ring) :c))
    (assert (= (.size ring) 0))))
