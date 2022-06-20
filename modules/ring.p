; ring bufer module.

(class Ring ()
  buf rdpos)

(method Ring .init (size)
  (&buf! self (array (++ size)))
  (&rdpos! self 0)
  (&wrpos! self 0))

(method Ring .samePos? ()
  (= (&wrpos self) (&rdpos self)))

(method Ring .nextPos (pos)
  (% (++ pos) (len (&buf self))))

(method Ring .get ()
  (if (.samePos? self) (raise StateError)
      (let (rdpos (&rdpos self))
        (&rdpos! self (.nextPos self rdpos))
        ([] (&buf self) rdpos))))

(method Ring .put (x)
  (let (wrpos (&wrpos self))
    ([] (&buf self) wrpos x)
    (&wrpos! self (.nextPos self wrpos))
    (if (.samePos? self) (raise StateError)
        self)))

(method Ring .size ()
  (let (wrpos (&wrpos self) rdpos (&rdpos self))
    (if (<= rdpos wrpos) (- wrpos rdpos)
        (+ (- (len (&buf self)) rdpos) wrpos))))

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
