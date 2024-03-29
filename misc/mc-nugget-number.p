; McNuggets numbers.

(<- $nuggets '(20 9 6))

(function combination (N x :opt acc)
  (if (nil? N) nil
      (= x 0) acc
      (> x 0) (|| (combination N (- x (car N)) (cons (car N) acc))
                  (combination (cdr N) x acc))))

(function! main (args)
  (let (x nil max 0)
    (dolist (n (cdr (.. 100)))
      (if (nil? (<- x (combination $nuggets n))) (<- max n))
      (printf "%3d: %v\n" n x))
    (printf "largest non-McNuggets number is %d\n" max)))
