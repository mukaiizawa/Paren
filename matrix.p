; matrix module.

(import :point)

(class Matrix ()
  width height contents)

(method Matrix .init (:key point)
  (with (m self)
    (&width! m (.x point))
    (&height! m (.y point))
    (&contents! m (array (* (&width self) (&height self))))))

(method Matrix .width ()
  (&width self))

(method Matrix .height ()
  (&height self))

(method Matrix .inside? (p)
  (and (<= 0 (.x p) (-- (&width self)))
       (<= 0 (.y p) (-- (&height self)))))

(method Matrix .index (p)
  (if (not (.inside? self p)) (error "index out of bounds " (.to-s p))
      (+ (.x p) (* (.y p) (&width self)))))

(method Matrix .at (p)
  (nth (&contents self) (.index self p)))

(method Matrix .at! (p v)
  (with (m self)
    (nth! (&contents m) (.index m p) v)))

(macro domatrix ((p m) :rest body)
  (with-gensyms (gx gy)
    (list 'dotimes (list gx (list '&width m))
          (list 'dotimes (list gy (list '&height m))
                (list let (list p (list '.init (list '.new 'Point) :x gx :y gy))
                      (cons begin body))))))

(function! main ()
  (with (m (.init (.new Matrix) :point (point 2 3)))
    (assert (= (&width m) 2))
    (assert (= (&height m) 3))
    (assert (nil? (.at m (point 0 0))))
    (domatrix (p m)
      (.at! m p (.x p)))
    (domatrix (p m)
      (assert (= (.at m p) (.x p))))))
