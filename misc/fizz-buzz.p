; fizz buzz.

(function! main (args)
  (let (xxxx? (f (x) (f (y) (= (% y x) 0)))
              fizz? (xxxx? 3) buzz? (xxxx? 5))
    (foreach (f (x)
               (if (|| (fizz? x) (buzz? x))
                   (begin
                     (if (fizz? x) (write-bytes "fizz"))
                     (if (buzz? x) (write-bytes "buzz"))
                     (write-line))
                   (write x)))
             (.. 1 100))))
