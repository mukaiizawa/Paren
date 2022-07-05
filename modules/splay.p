; splay module.

(class SplayNode ()
  key val left right)

(method SplayNode .init (:opt key val left right)
  (<- self->key key
      self->val val
      self->left left
      self->right right)
  self)

(method SplayNode .rotl ()
  (let (p self->left)
    (<- self->left p->right p->right self)
    p))

(method SplayNode .rotr ()
  (let (p self->right)
    (<- self->right p->left p->left self)
    p))

(method SplayNode .rotll ()
  (let (p self->left q p->left)
    (<- p->left q->right q->right self)
    q))

(method SplayNode .rotrr ()
  (let (p self->right q p->right)
    (<- p->right q->left q->left self)
    q))

(method SplayNode .rotlr ()
  (let (p self->left q p->right)
    (<- p->right q->left q->left p
        self->left q->right q->right self)
    q))

(method SplayNode .rotrl ()
  (let (p self->right q p->left)
    (<- p->left q->right q->right p
        self->right q->left q->left self)
    q))

(class Splay ()
  top sentinel cmp size)

(method Splay .init (cmp)
  ; Initialize the receiver using the comparison function cmp, which returns a negative, zero, or positive integer.
  ; Returns the receiver.
  (let (sentinel (.new SplayNode))
    (<- self->top sentinel
        self->sentinel sentinel
        self->cmp cmp
        self->size 0)
    self))

(method Splay .balance (key)
  (let (top self->top sentinel self->sentinel cmp self->cmp p top q nil d nil)
    (<- sentinel->key key
        sentinel->left sentinel
        sentinel->right sentinel)
    (while (!= (<- d (cmp p->key key)) 0)
      (if (< d 0)
          (begin
            (<- q p->left)
            (if (= (<- d (cmp q->key key)) 0)
                (begin (<- p (.rotl p)) (break))
                (<- p (if (< d 0) (.rotll p) (.rotlr p)))))
          (begin
            (<- q p->right)
            (if (= (<- d (cmp q->key key)) 0)
                (begin (<- p (.rotr p)) (break))
                (<- p (if (> d 0) (.rotrr p) (.rotrl p)))))))
    (<- self->top p)
    p))

(method Splay .resume ()
  (let (top self->top left top->left right top->right sentinel self->sentinel)
    (if (== left sentinel) (<- self->top right)
        (== right sentinel) (<- self->top left)
        (let (p left)
          (while (!= p->right sentinel) (<- p p->right))
          (<- p->right right self->top left)))
    nil))

(method Splay .has-key? (key)
  ; Returns whether the value corresponding to the key exists.
  (let (top (.balance self key) absent? (== top self->sentinel))
    (if absent? (.resume self)
        true)))

(method Splay .size ()
  ; Returns the number of key-value mappings in the receiver.
  self->size)

(method Splay .get (key)
  ; Returns the value corresponding to key.
  ; If there is no corresponding value for the key, returns nil.
  (let (top (.balance self key) sentinel self->sentinel)
    (if (== top sentinel) (.resume self)
        top->val)))

(method Splay .put (key val)
  ; Set the value corresponding to the key.
  ; Works with or without a corresponding value already.
  ; Returns the receiver.
  (let (top (.balance self key) sentinel self->sentinel)
    (if (== top sentinel) (<- self->top (.init (.new SplayNode) key val sentinel->left sentinel->right)
                              self->size (++ self->size))
        (<- top->val val))))

(method Splay .remove (key)
  ; Delete the value corresponding to the key.
  ; If there is no corresponding value for the key, do nothing.
  ; Returns the receiver.
  (let (top (.balance self key))
    (if (!== top self->sentinel) (<- self->size (-- self->size)))
    (.resume self)
    self))

(method Splay .foreach (fn)
  ; Call a function fn that takes a key and a value as arguments for each node in splay.
  (let (sentinel self->sentinel
                 rec (f (node)
                       (when (!== sentinel node)
                         (rec node->left)
                         (fn node->key node->val)
                         (rec node->right))))
    (rec self->top)))

(function! main (args)
  (let (splay (.init (.new Splay) symcmp))
    (.put splay :one 1)
    (.put splay :two 2)
    (.put splay :three 3)
    ;; (:one 1 :two 2 :three 3)
    (assert (= (.size splay) 3))
    (assert (.has-key? splay :three))
    (assert (! (.has-key? splay :four)))
    (assert (nil? (.get splay :zero)))
    (assert (= (.get splay :one) 1))
    (assert (= (.get splay :two) 2))
    (assert (= (.get splay :three) 3))
    (assert (nil? (.get splay :four)))
    (.put splay :one "1")
    ;; (:one "1" :two 2 :three 3)
    (assert (= (.size splay) 3))
    (assert (= (.get splay :one) "1"))
    (.remove splay :one)
    ;; (:two 2 :three 3)
    (assert (= (.size splay) 2))
    (.remove splay :two)
    ;; (:three 3)
    (assert (= (.size splay) 1))
    (.remove splay :four)
    (assert (= (.size splay) 1))
    (assert (nil? (.get splay :two)))
    (assert (= (.get splay :three) 3))
    (assert (nil? (.get splay :four)))
    (let (key-set nil)
      (.foreach splay (f (key val) (push! key key-set)))
      (assert (= (len key-set) 1))
      (assert (== (car key-set) :three)))))
