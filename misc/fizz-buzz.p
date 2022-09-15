; fizz buzz.

(function! main (args)
  (let (xxxx? (f (x) (f (y) (= (% y x) 0))) fizz? (xxxx? 3) buzz? (xxxx? 5))
    (dolist (i (.. 1 100))
      (if (&& (! (fizz? i)) (! (buzz? i))) (println i)
          (println (if (fizz? i) "fizz") (if (buzz? i) "buzz"))))))
