; Comparable feature module.

(class Comparable ()
  ; A feature that provides comparison operators.
  )

(method Comparable .cmp (:rest args)
  ; Compares the receiver with the specified object.
  ; Returns a negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
  (raise NotImplementedError))

(method Comparable .eq? (o)
  ; Returns whether the receiver and o is equal.
  (= (.cmp self o) 0))
