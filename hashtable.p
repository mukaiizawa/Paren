; hash table module.

(class HashTable ()
  hash eq? table table-size size)

(method HashTable .init (hash eq?)
  ; Initialize using the key hash function hash and the comparison function eq?.
  ; Returns the receiver.
  (let (table-size 0xf)
    (&size! self 0)
    (&table-size! self table-size)
    (&table! self (array table-size))
    (&eq?! self eq?)
    (&hash! self hash)))

(method HashTable .size ()
  ; Returns the number of elements in the receiver.
  (&size self))

(method HashTable .reset ()
  ; Clear the element in the receiver.
  ; Returns the receiver.
  (dotimes (i (&table-size self))
    ([] (&table self) i nil))
  (&size! self 0))

(method HashTable .table-index (key)
  (let (hash (&hash self))
    (mod (hash key) (&table-size self))))

(method HashTable .get (key)
  ; Returns the value corresponding to the key.
  ; Returns nil if there is no corresponding value for the key.
  (let (eq? (&eq? self))
    (dolist (node ([] (&table self) (.table-index self key)))
      (if (eq? key (car node)) (return (cadr node))))
    nil))

(method HashTable .rehash ()
  (write-line :rehash)
  (let (old-table (&table self) new-table-size (<< (&table-size self) 1))
    (&table-size! self new-table-size)
    (&table! self (array new-table-size))
    (&size! self 0)
    (doarray (nodes old-table)
      (dolist (node nodes)
        (.put self (car node) (cadr node))))
    self))

(method HashTable .put (key val)
  ; Associate a value with a key.
  (let (table (&table self) i (.table-index self key) root-node ([] table i) eq? (&eq? self))
    (dolist (node root-node)
      (when (eq? key (car node))
        (car! (cdr node) val)
        (return self)))
    ([] table i (cons (list key val) root-node))
    (let (size (++ (&size self)))
      (&size! self size)
      (if (> size (// (* (&table-size self) 3) 4)) (.rehash self)
          self))))

(function! main (args)
  (let (ht (.init (.new HashTable) address eq?))
    (assert (= (.size (.reset ht)) 0))
    (assert (= (.size ht) 0))
    (assert (nil? (.get ht :foo)))
    (assert (memeq? (.get (.put ht :foo "foo") :foo) "foo"))
    (assert (= (.size ht) 1))
    (assert (memeq? (.get (.put ht :bar "bar") :bar) "bar"))
    (assert (= (.size ht) 2))
    (assert (eq? (.get (.put ht :buzz 'buzz) :buzz) 'buzz))
    (assert (= (.size ht) 3))
    (assert (memeq? (.get (.put ht :buzz "buzz") :buzz) "buzz"))
    (assert (= (.size ht) 3)))
  (let (ht (.init (.new HashTable) memhash memeq?))
    (assert (= (.size (.reset ht)) 0))
    (assert (= (.size ht) 0))
    (assert (nil? (.get ht :foo)))
    (assert (memeq? (.get (.put ht "foo" "foo") :foo) "foo"))
    (assert (= (.size ht) 1))
    (assert (memeq? (.get (.put ht "bar" "bar") :bar) "bar"))
    (assert (= (.size ht) 2))
    (assert (eq? (.get (.put ht "buzz" 'buzz) :buzz) 'buzz))
    (assert (= (.size ht) 3))
    (assert (memeq? (.get (.put ht "buzz" "buzz") :buzz) "buzz"))
    (assert (= (.size ht) 3))))
