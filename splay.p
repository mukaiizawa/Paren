; splay module.

(class SplayNode ()
  key val left right)

(method SplayNode .init (:opt key val left right)
  (&key! self key)
  (&val! self val)
  (&left! self left)
  (&right! self right))

(method SplayNode .rotl ()
  (let (p (&left self))
    (&left! self (&right p))
    (&right! p self)
    p))

(method SplayNode .rotr ()
  (let (p (&right self))
    (&right! self (&left p))
    (&left! p self)
    p))

(method SplayNode .rotll ()
  (let (p (&left self) q (&left p))
    (&left! p (&right q))
    (&right! q self)
    q))

(method SplayNode .rotrr ()
  (let (p (&right self) q (&right p))
    (&right! p (&left q))
    (&left! q self)
    q))

(method SplayNode .rotlr ()
  (let (p (&left self) q (&right p))
    (&right! p (&left q))
    (&left! q p)
    (&left! self (&right q))
    (&right! q self)
    q))

(method SplayNode .rotrl ()
  (let (p (&right self) q (&left p))
    (&left! p (&right q))
    (&right! q p)
    (&right! self (&left q))
    (&left! q self)
    q))

(class Splay ()
  top sentinel cmp size)

(method Splay .init (cmp)
  ; Initialize the receiver using the comparison function cmp, which returns a negative, zero, or positive integer.
  ; Returns the receiver.
  (let (sentinel (.new SplayNode))
    (&top! self sentinel)
    (&sentinel! self sentinel)
    (&cmp! self cmp)
    (&size! self 0)))

(method Splay .balance (key)
  (let (top (&top self) sentinel (&sentinel self) cmp (&cmp self) p top q nil d nil)
    (&key! sentinel key)
    (&left! sentinel sentinel)
    (&right! sentinel sentinel)
    (while (/= (<- d (cmp (&key p) key)) 0)
      (if (< d 0)
          (begin (<- q (&left p))
                 (if (= (<- d (cmp (&key q) key)) 0)
                     (begin (<- p (.rotl p)) (break))
                     (<- p (if (< d 0) (.rotll p) (.rotlr p)))))
          (begin (<- q (&right p))
                 (if (= (<- d (cmp (&key q) key)) 0)
                     (begin (<- p (.rotr p)) (break))
                     (<- p (if (> d 0) (.rotrr p) (.rotrl p)))))))
    (&top! self p)
    p))

(method Splay .resume ()
  (let (top (&top self) left (&left top) right (&right top) sentinel (&sentinel self))
    (if (eq? left sentinel) (&top! self right)
        (eq? right sentinel) (&top! self left)
        (let (p left)
          (while (neq? (&right p) sentinel) (<- p (&right p)))
          (&right! p right)
          (&top! self left)))
    nil))

(method Splay .has-key? (key)
  ; Returns whether the value corresponding to the key exists.
  (let (top (.balance self key) absent? (eq? top (&sentinel self)))
    (if absent? (.resume self)
        true)))

(method Splay .size ()
  ; Returns the number of key-value mappings in the receiver.
  (&size self))

(method Splay .get (key)
  ; Returns the value corresponding to key.
  ; If there is no corresponding value for the key, returns nil.
  (let (top (.balance self key) sentinel (&sentinel self))
    (if (eq? top sentinel) (.resume self)
        (&val top))))

(method Splay .put (key val)
  ; Set the value corresponding to the key.
  ; Works with or without a corresponding value already.
  ; Returns the receiver.
  (let (top (.balance self key) sentinel (&sentinel self))
    (if (eq? top sentinel) (begin
                             (&top! self (.init (.new SplayNode) key val (&left sentinel) (&right sentinel)))
                             (&size! self (++ (&size self))))
        (&val! top val))))

(method Splay .remove (key)
  ; Delete the value corresponding to the key.
  ; If there is no corresponding value for the key, do nothing.
  ; Returns the receiver.
  (let (top (.balance self key))
    (if (neq? top (&sentinel self)) (&size! self (-- (&size self))))
    (.resume self)
    self))

(method Splay .foreach (fn)
  ; Call a function fn that takes a key and a value as arguments for each node in splay.
  (let (sentinel (&sentinel self)
                 rec (f (node)
                       (when (neq? sentinel node)
                         (rec (&left node))
                         (fn (&key node) (&val node))
                         (rec (&right node)))))
    (rec (&top self))))

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
    (assert (memeq? (.get splay :one) "1"))
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
      (assert (= (length key-set) 1))
      (assert (eq? (car key-set) :three)))))
