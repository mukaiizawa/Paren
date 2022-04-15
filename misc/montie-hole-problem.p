; Monty Hall problem.

(import :rand)

(<- N 10000)

(function! main (args)
  (let (stay-win-count 0 switch-win-count 0) 
    (dotimes (i N)
      (let (prize-pos (rand.int 3) choice (rand.int 3))
        (if (= prize-pos choice) (<- stay-win-count (++ stay-win-count))
            (<- switch-win-count (++ switch-win-count)))))
    (write (list :without-change (/ stay-win-count N)
                 :change (/ switch-win-count N)))))
