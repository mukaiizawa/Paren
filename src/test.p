(def sigma)
(<- sigma (fn (x) (ifElse (=? x 1) 1
                          (+ x (sigma (+ x -1))))))
(sigma 10)
