; splay

(class Splay ()
  comparator top)

(function splay-new (:opt (comparator
                            (lambda (k1 k2)
                              (- (address k1) (address k2)))))
  ; Returns spray tree.
  (cons comparator $splay-nil))

(function splay-top (splay)
  (cdr splay))

(function splay-top! (splay top)
  (cdr! splay top))

(function splay-comparator (splay)
  (car splay))

(function splay-node-new (k v l r)
  (cons (cons k l) (cons v r)))

(function splay-node-key (splay-node)
  (caar splay-node))

(function splay-node-key! (splay-node key)
  (car! (car splay-node) key))

(function splay-node-val (splay-node)
  (cadr splay-node))

(function splay-node-left (splay-node)
  (cdar splay-node))

(function splay-node-left! (splay-node val)
  (cdr! (car splay-node) val))

(function splay-node-right (splay-node)
  (cddr splay-node))

(function splay-node-right! (splay-node val)
  (cdr! (cdr splay-node) val))

(function splay-balance (splay k)
  (let (top (splay-top splay) cmp (splay-comparator splay) p nil q nil d 0)
    (splay-node-key! $splay-nil k)
    (splay-node-left! $splay-nil $splay-nil)
    (splay-node-right! $splay-nil $splay-nil)
    (while (/= (<- d (cmp k (splay-node-key top))) 0)
      (<- p top)
      (if (< d 0)
          (begin
            (<- q (splay-node-left p))
            (if (= (<- d (cmp k (splay-node-key q))) 0)
                (begin
                  (<- top q)
                  (splay-node-left! p (splay-node-right top))
                  (splay-node-right! top p)
                  (break))
                (< d 0)
                (begin
                  (<- top (splay-node-left q))
                  (splay-node-left! q (splay-node-right top))
                  (splay-node-right! top p))
                (begin
                  (<- top (splay-node-right q))
                  (splay-node-right! q (splay-node-left top))
                  (splay-node-left! top q)
                  (splay-node-left! p (splay-node-right top))
                  (splay-node-right! top p))))
          (begin
            (<- q (splay-node-right p))
            (if (= (<- d (cmp k (splay-node-key q))) 0)
                (begin
                  (<- top q)
                  (splay-node-right! p (splay-node-left top))
                  (splay-node-left! top p)
                  (break))
                (> d 0)
                (begin
                  (<- top (splay-node-right q))
                  (splay-node-right! q (splay-node-left top))
                  (splay-node-left! top p))
                (begin
                  (<- top (splay-node-left q))
                  (splay-node-left! q (splay-node-right top))
                  (splay-node-right! top q)
                  (splay-node-right! p (splay-node-left top))
                  (splay-node-left! top p))))))
    top))

(function splay-resume (top)
  (let (l (splay-node-left top) r (splay-node-right top) p nil)
    (if (same? l $splay-nil) (return r)
        (different? r $splay-nil)
        (begin (<- p l)
               (while (different? (splay-node-right p) $splay-nil)
                 (<- p (splay-node-right p)))
               (splay-node-right! p r)))
    l))

(function splay-add (splay k v)
  ; Associates the specified v with the specified k in the specified splay.
  ; Returns the v.
  (let (top (splay-balance splay k))
    (assert (same? top $splay-nil))
    (splay-top! splay (splay-node-new k v (splay-node-left $splay-nil)
                                      (splay-node-right $splay-nil)))
    v))

(function splay-find (splay k)
  ; Returns the value to which the specified k is associated in the specified splay.
  ; If not found, return nil.
  (let (top (splay-balance splay k))
    (if (same? top $splay-nil) (begin (splay-top! splay (splay-resume top))
                                      nil)
        (begin (splay-top! splay top)
               (splay-node-val top)))))

(global-symbol $splay-nil (splay-node-new nil nil nil nil)
  ; End node of splay.
  ; Be careful when handling because it will be a circular list.
  )

(function main ()
  (assert nil))
