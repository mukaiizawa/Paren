; hash table module.

(class HashTable ()
  hash eq? table table-size size)

(method HashTable .init (hash eq?)
  ; Initialize using the key hash function hash and the comparison function eq?.
  ; Returns the receiver.
  (let (table-size 0xf)
    (<- self->size 0
        self->table-size table-size
        self->table (array table-size)
        self->eq? eq?
        self->hash hash)
    self))

(method HashTable .size ()
  ; Returns the number of elements in the receiver.
  self->size)

(method HashTable .reset ()
  ; Clear the element in the receiver.
  ; Returns the receiver.
  (dotimes (i self->table-size)
    ([] self->table i nil))
  (<- self->size 0)
  self)

(method HashTable .table-index (key)
  (let (hash self->hash)
    (% (hash key) self->table-size)))

(method HashTable .get (key)
  ; Returns the value corresponding to the key.
  ; Returns nil if there is no corresponding value for the key.
  (let (eq? self->eq?)
    (dolist (node ([] self->table (.table-index self key)))
      (if (eq? key (car node)) (return (cadr node))))
    nil))

(method HashTable .rehash ()
  (let (old-table self->table new-table-size (<< self->table-size 1))
    (<- self->table-size new-table-size)
    (<- self->table (array new-table-size))
    (<- self->size 0)
    (doarray (nodes old-table)
      (dolist (node nodes)
        (.put self (car node) (cadr node))))
    self))

(method HashTable .put (key val)
  ; Associate a value with a key.
  (let (table self->table i (.table-index self key) root-node ([] table i) eq? self->eq?)
    (dolist (node root-node)
      (when (eq? key (car node))
        (car! (cdr node) val)
        (return self)))
    ([] table i (cons (list key val) root-node))
    (let (size (++ self->size))
      (<- self->size size)
      (if (> size (// (* self->table-size 3) 4)) (.rehash self)
          self))))

(function! main (args)
  (let (ht (.init (.new HashTable) address ==))
    (assert (= (.size (.reset ht)) 0))
    (assert (= (.size ht) 0))
    (assert (nil? (.get ht :foo)))
    (assert (= (.get (.put ht :foo :foo) :foo) :foo))
    (assert (= (.size ht) 1))
    (assert (= (.get (.put ht :bar :bar) :bar) :bar))
    (assert (= (.size ht) 2))
    (assert (= (.get (.put ht :buzz :buzz) :buzz) :buzz))
    (assert (= (.size ht) 3))
    (assert (= (.get (.put ht :buzz :buzz) :buzz):buzz))
    (assert (= (.size ht) 3))))
