; disjoint-set forest.

(class DisjointSet.Node ()
  parent-node-index
  set-size)

(method DisjointSet.Node .init (index)
  (<- self->parent-node-index index
      self->set-size 1)
  self)

(class DisjointSet ()
  size nodes)

(method DisjointSet .init (size)
  (<- self->size size
      self->nodes (array size))
  (dotimes (i size)
    ([] self->nodes i (.init (.new DisjointSet.Node) i)))
  self)

(method DisjointSet .size ()
  ; Returns the number of sets.
  ; This value is monotonically decremented from the value initialized by `.init` each time a set is merged by a call to the `.union` method.
  self->size)

(method DisjointSet .size-at (i)
  ; Returns the size of the set to which the i-th node belongs.
  ; This value is monotonically increased each time the set is merged by a call to the `.union` method.
  (let (representative-node ([] self->nodes (.find self i)))
    representative-node->set-size))

(method DisjointSet .union (i j)
  ; Merge the sets to which the i-th node and the j-th node belong.
  ; If they already belong to the same set, nothing is done.
  ; Returns whether or not the i-th node and the j-th node belong to the same set.
  (let ((i j) (map (partial .find self) (list i j)))
    (if (= i j) nil
        (let ((src dst) (map (partial [] self->nodes) (list i j)))
          (if (< dst->set-size src->set-size) (<- (i j src dst) (list j i dst src)))
          (<- dst->set-size (+ src->set-size dst->set-size)
              src->parent-node-index j
              self->size (-- self->size))
          true))))

(method DisjointSet .find (i)
  ;; Returns the representative node index of the set to which the i-th node belongs.
  (let (node ([] self->nodes i) parent-node-index node->parent-node-index)
    (if (= i parent-node-index) i
        (let (parent-node ([] self->nodes parent-node-index))
          (.find self (<- node->parent-node-index parent-node->parent-node-index))))))

(method DisjointSet .in-same-set? (i j)
  ; Returns whether the i-th node and the j-th node belong to the same set.
  (apply = (map (partial .find self) (list i j))))

(function! main (args)
  (let (sets (.init (.new DisjointSet) 10))
    ; ((0) (1) (2) (3) (4) (5) (6) (7) (8) (9))
    (assert (= (.size sets) 10))
    (dotimes (i 10)
      (assert (.in-same-set? sets i i))
      (assert (= (.size-at sets i) 1)))
    ;; merge 0, 1, 2
    (assert (! (.union sets 0 0)))
    (assert (.union sets 0 1))
    (assert (.union sets 0 2))
    (assert (! (.union sets 1 2)))
    ; ((0 1 2) (3) (4) (5) (6) (7) (8) (9))
    (assert (= (.size sets) 8))
    (assert (.in-same-set? sets 0 1))
    (assert (.in-same-set? sets 1 2))
    (assert (.in-same-set? sets 0 2))
    (assert (! (.in-same-set? sets 1 7)))
    (assert (= (.size-at sets 0) 3))
    (assert (= (.size-at sets 1) 3))
    (assert (= (.size-at sets 2) 3))
    ;; merge 3, 7, 8, 9
    (assert (! (.union sets 7 7)))
    (assert (.union sets 7 9))
    (assert (.union sets 9 8))
    (assert (.union sets 3 8))
    ; ((0 1 2) (4) (5) (6) (3 7 8 9))
    (assert (= (.size sets) 5))
    (assert (= (.size-at sets 3) 4))
    (assert (.in-same-set? sets 3 9))
    (assert (! (.in-same-set? sets 2 3)))
    ;; merge 4, 5, 6
    (assert (.union sets 5 4))
    (assert (.union sets 6 5))
    (assert (.union sets 1 4))
    ; ((0 1 2 4 5 6) (3 7 8 9))
    (assert (= (.size sets) 2))
    (assert (.in-same-set? sets 0 4))
    (assert (! (.in-same-set? sets 2 9)))
    (dotimes (i 10)
      (assert (= (.size-at sets i) (if (in? i '(3 7 8 9)) 4 6))))
    ;; merge all
    (assert (! (.union sets 4 5)))
    (assert (! (.union sets 3 9)))
    (assert (.union sets 6 8))
    (assert (! (.union sets 8 6)))
    ; ((0 1 2 3 4 5 6 7 8 9))
    (assert (= (.size sets) 1))
    (dotimes (i 10)
      (dotimes (j 10)
        (assert (.in-same-set? sets i j)))
      (assert (= (.size-at sets i) 10)))))
