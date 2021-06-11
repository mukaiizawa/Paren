; matrix module.

(class Matrix ()
  shape contents)

(function matrix (shape)
  ; Returns a matrix instance corresponding to the shape (x y).
  (.init (.new Matrix) shape))

(method Matrix .init (shape)
  ; Initialize the receiver to a matrix object with width x height y based on a point object that has coordinates (x, y).
  (if (|| (!= (len shape) 2) (some? (f (x) (<= x 0)) shape)) (raise ArgumentError))
  (&shape! self shape)
  (&contents! self (array (apply * shape))))

(method Matrix .shape ()
  ; Returns the elements of the shape list give the lengths of the corresponding the receiver dimensions.
  (&shape self))

(method Matrix .inside? (p)
  ; Returns whether the position corresponding to the point is within the receiver.
  (let ((x y) (&shape self))
    (&& (<= 0 (car p) (-- x))
        (<= 0 (cadr p) (-- y)))))

(method Matrix .index (p)
  (if (! (.inside? self p)) (raise IndexError)
      (+ (car p) (* (cadr p) (car (&shape self))))))

(method Matrix .at (p)
  ; Returns the value of receiver at the position corresponding to the point object.
  ([] (&contents self) (.index self p)))

(method Matrix .put (p v)
  ; Update the value of the receiver at the position corresponding to the point object to v.
  ; Returns the receiver.
  ([] (&contents self) (.index self p) v)
  self)

(macro domatrix ((p m) :rest body)
  ; Iterate the entire matrix m with the specified symbol p.
  ; Returns nil.
  (with-gensyms (gi gj gx gy)
    `(let ((,gx ,gy) (.shape ,m))
       (dotimes (,gi ,gx)
         (dotimes (,gj ,gy)
           (let (,p (list ,gi ,gj))
             ,@body))))))

(function! main (args)
  (let (m (matrix '(2 3)))
    (assert (= (.shape m) '(2 3)))
    (domatrix (p m) (.put m p p))
    (domatrix (p m) (assert (= (.at m p) p)))))
