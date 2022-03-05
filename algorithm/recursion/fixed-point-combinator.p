; Fixed-point combinator.

(function Y (fn)
  (f (x) (apply (fn (Y fn)) (list x))))

(function! main (args)
  ; 5! => 120
  (assert (= (apply (Y (f (fn) (f (n) (if (= n 0) 1 (* n (fn (-- n))))))) '(5)) 120)))
