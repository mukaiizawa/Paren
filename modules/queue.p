; queue module.

(class Queue ()
  ; FIFO Queue.
  size head tail)

(method Queue .init ()
  (&size! self 0))

(method Queue .empty? ()
  ; Returns whether the receiver is empty.
  (= (&size self) 0))

(method Queue .size ()
  ; Returns the size of the receiver.
  (&size self))

(method Queue .enqueue (x)
  ; Add element x.
  ; Returns the receiver.
  (let (tail (&tail self))
    (&tail! self (cons x nil))
    (&size! self (++ (&size self)))
    (if (nil? tail) (&head! self (&tail self))
        (cdr! tail (&tail self)))
    self))

(method Queue .dequeue ()
  ; Returns the first element.
  ; If the receiver is empty, returns nil.
  (let (head (&head self))
    (if (nil? head) nil
        (begin
          (if (== head (&tail self)) (&tail! self nil))
          (&head! self (cdr head))
          (&size! self (-- (&size self)))
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
