; matrix module.

(import :point)

(class Matrix ()
  width height contents)

(method Matrix .init (p)
  ; Initialize the receiver to a matrix object with width x height y based on a point object that has coordinates (x, y).
  (if (! (is-a? p Point)) (raise ArgumentError "expected instance of Point"))
  (&width! self (.x p))
  (&height! self (.y p))
  (&contents! self (array (* (&width self) (&height self)))))

(method Matrix .width ()
  ; Returns width of the receiver.
  (&width self))

(method Matrix .height ()
  ; Returns height of the receiver.
  (&height self))

(method Matrix .inside? (p)
  ; Returns whether the position corresponding to the point object p is within the receiver.
  (assert (is-a? p Point))
  (&& (<= 0 (.x p) (-- (&width self)))
      (<= 0 (.y p) (-- (&height self)))))

(method Matrix .index (p)
  (if (! (.inside? self p)) (raise IndexError)
      (+ (.x p) (* (.y p) (&width self)))))

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
  (with-gensyms (gx gy)
    `(dotimes (,gx (&width ,m))
       (dotimes (,gy (&height ,m))
         (let (,p (point ,gx ,gy))
           ,@body)))))

(function! main (args)
  (let (m (.init (.new Matrix) (point 2 3)))
    (assert (= (&width m) 2))
    (assert (= (&height m) 3))
    (domatrix (p m) (.put m p (.x p)))
    (domatrix (p m) (assert (= (.at m p) (.x p))))))
