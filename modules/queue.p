; queue module.

(class Queue ()
  ; FIFO Queue.
  size head tail)

(method Queue .init ()
  (<- self->size 0)
  self)

(method Queue .empty? ()
  ; Returns whether the receiver is empty.
  (= self->size 0))

(method Queue .size ()
  ; Returns the size of the receiver.
  self->size)

(method Queue .enqueue (x)
  ; Add element x.
  ; Returns the receiver.
  (let (tail self->tail)
    (<- self->tail (cons x nil)
        self->size (++ self->size))
    (if (nil? tail) (<- self->head self->tail)
        (cdr! tail self->tail))
    self))

(method Queue .dequeue ()
  ; Returns the first element.
  ; If the receiver is empty, returns nil.
  (let (head self->head)
    (if (nil? head) nil
        (begin
          (if (== head self->tail) (<- self->tail nil))
          (<- self->head (cdr head))
          (<- self->size (-- self->size))
          (car head)))))

(function! main (args)
  (let (q (.new Queue))
    (.enqueue q 1)
    (.enqueue q 2)
    (.enqueue q 3)
    (assert (= (.size q) 3))
    (assert (nil? (.empty? q)))
    (assert (= (.dequeue q) 1))
    (assert (= (.dequeue q) 2))
    (assert (= (.dequeue q) 3))
    (assert (= (.size q) 0))
    (assert (.empty? q))
    (assert (nil? (.dequeue q)))
    (.enqueue q :one)
    (.enqueue q :two)
    (assert (= (.dequeue q) :one))))
