; matrix module.

(import :point)

(class Matrix ()
  width height contents)

(method Matrix .init (p)
  (&width<- self (.x p))
  (&height<- self (.y p))
  (&contents<- self (array (* (&width self) (&height self)))))

(method Matrix .width ()
  (&width self))

(method Matrix .height ()
  (&height self))

(method Matrix .inside? (p)
  (&& (<= 0 (.x p) (-- (&width self)))
      (<= 0 (.y p) (-- (&height self)))))

(method Matrix .index (p)
  (if (! (.inside? self p)) (error "index out of bounds " (.to-s p))
      (+ (.x p) (* (.y p) (&width self)))))

(method Matrix .at (p)
  ([] (&contents self) (.index self p)))

(method Matrix .put (p v)
  ([]<- (&contents self) (.index self p) v)
  self)

(macro domatrix ((p m) :rest body)
  (with-gensyms (gx gy)
    (list 'dotimes (list gx (list '&width m))
          (list 'dotimes (list gy (list '&height m))
                (list let (list p (list 'Point.of gx gy))
                      (cons begin body))))))

(function! main (args)
  (let (m (.init (.new Matrix) (Point.of 2 3)))
    (assert (= (&width m) 2))
    (assert (= (&height m) 3))
    (domatrix (p m)
       (.put m p (.x p)))
    (domatrix (p m)
       (assert (= (.at m p) (.x p))))))
