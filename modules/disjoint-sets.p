; disjoint sets.

(class DisjointSets.Node ()
  parent-node-index
  set-size)

(method DisjointSets.Node .init (index)
  (<- self->parent-node-index index
      self->set-size 1)
  self)

(class DisjointSets ()
  set-count nodes)

(method DisjointSets .init (size)
  (<- self->set-count size
      self->nodes (array size))
  (dotimes (i size)
    ([] self->nodes i (.init (.new DisjointSets.Node) i)))
  self)

(method DisjointSets .set-count ()
  ; Returns the number of sets.
  ; This value is monotonically decremented from the value initialized by `.init` each time a set is merged by a call to the `.merge` method.
  self->set-count)

(method DisjointSets .set-size (i)
  ; Returns the size of the set to which the i-th node belongs.
  ; This value is monotonically increased each time the set is merged by a call to the `.merge` method.
  (let (representative-node ([] self->nodes (.represenive-node-index self i)))
    representative-node->set-size))

(method DisjointSets .represenive-node-index (i)
  ;; Returns the representative node index of the set to which the i-th node belongs.
  (let (node ([] self->nodes i) parent-node-index node->parent-node-index)
    (if (= i parent-node-index) i
        (let (parent-node ([] self->nodes parent-node-index))
          (.represenive-node-index self (<- node->parent-node-index parent-node->parent-node-index))))))

(method DisjointSets .in-same-set? (i j)
  ; Returns whether the i-th node and the j-th node belong to the same set.
  (apply = (map (partial .represenive-node-index self) (list i j))))

(method DisjointSets .merge (i j)
  ; Merge the sets to which the i-th node and the j-th node belong.
  ; If they already belong to the same set, nothing is done.
  ; Returns whether or not the i-th node and the j-th node belong to the same set.
  (let ((i j) (map (partial .represenive-node-index self) (list i j)))
    (if (= i j) nil
        (let ((src dst) (map (partial [] self->nodes) (list i j)))
          (if (< dst->set-size src->set-size) (<- (i j src dst) (list j i dst src)))
          (<- dst->set-size (+ src->set-size dst->set-size)
              src->set-size 0
              src->parent-node-index j
              self->set-count (-- self->set-count))
          true))))

(function! main (args)
  (let (sets (.init (.new DisjointSets) 10))
    ; ((0) (1) (2) (3) (4) (5) (6) (7) (8) (9))
    (assert (= (.set-count sets) 10))
    (dotimes (i 10)
      (assert (.in-same-set? sets i i))
      (assert (= (.set-size sets i) 1)))
    ;; merge 0, 1, 2
    (assert (! (.merge sets 0 0)))
    (assert (.merge sets 0 1))
    (assert (.merge sets 0 2))
    (assert (! (.merge sets 1 2)))
    ; ((0 1 2) (3) (4) (5) (6) (7) (8) (9))
    (assert (= (.set-count sets) 8))
    (assert (.in-same-set? sets 0 1))
    (assert (.in-same-set? sets 1 2))
    (assert (.in-same-set? sets 0 2))
    (assert (! (.in-same-set? sets 1 7)))
    (assert (= (.set-size sets 0) 3))
    (assert (= (.set-size sets 1) 3))
    (assert (= (.set-size sets 2) 3))
    ;; merge 3, 7, 8, 9
    (assert (! (.merge sets 7 7)))
    (assert (.merge sets 7 9))
    (assert (.merge sets 9 8))
    (assert (.merge sets 3 8))
    ; ((0 1 2) (4) (5) (6) (3 7 8 9))
    (assert (= (.set-count sets) 5))
    (assert (= (.set-size sets 3) 4))
    (assert (.in-same-set? sets 3 9))
    (assert (! (.in-same-set? sets 2 3)))
    ;; merge 4, 5, 6
    (assert (.merge sets 5 4))
    (assert (.merge sets 6 5))
    (assert (.merge sets 1 4))
    ; ((0 1 2 4 5 6) (3 7 8 9))
    (assert (= (.set-count sets) 2))
    (assert (.in-same-set? sets 0 4))
    (assert (! (.in-same-set? sets 2 9)))
    (dotimes (i 10)
      (assert (= (.set-size sets i) (if (in? i '(3 7 8 9)) 4 6))))
    ;; merge all
    (assert (! (.merge sets 4 5)))
    (assert (! (.merge sets 3 9)))
    (assert (.merge sets 6 8))
    (assert (! (.merge sets 8 6)))
    ; ((0 1 2 3 4 5 6 7 8 9))
    (assert (= (.set-count sets) 1))
    (dotimes (i 10)
      (dotimes (j 10)
        (assert (.in-same-set? sets i j)))
      (assert (= (.set-size sets i) 10)))))
