; comb sort.

(import :rand)

(<- $shrink 1.3)

(function comb-sort (seq)
  (let (sorted? nil gap (len seq))
    (while (! sorted?)
      (<- gap (max (int (/ gap $shrink)) 1)
          sorted? (= gap 1))
      (dotimes (i (- (len seq) gap))
        (if (< ([] seq (+ i gap)) ([] seq i))
            (begin
              (<- sorted? nil)
              (swap! seq i (+ i gap))))))
    seq))

(function! main (args)
  (assert (= (comb-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (assert (= (comb-sort '(1 1 3 3 5)) '(1 1 3 3 5)))
  (assert (= (comb-sort '(5 4 1 2 3)) '(1 2 3 4 5)))
  (assert (= (comb-sort (rand.shuffle! (.. 100))) (.. 100))))
