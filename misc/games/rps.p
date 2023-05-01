; Rock paper scissors.

(import :rand)

(<- $choices '("rock" "paper" "scissors"))

(function choice->index (c)
  (position (f (x) (prefix? x c)) $choices))

(function user-choice ()
  (print "r(ock), p(aper), s(cissors):\n>>")
  (let (choice (choice->index (read-line)))
    (if choice choice
        (begin
          (println "illegal choice")
          (user-choice)))))

(function computer-choice ()
  (rand.int 3))

(function rcp ()
  (let (user-choice (user-choice) computer-choice (computer-choice))
    (println "computer: " ([] $choices computer-choice))
    (if (= user-choice computer-choice) (println "draw")
        (= (% (++ user-choice) 3) computer-choice) (println "lose")
        (println "win"))
    (rcp)))

(function! main (args)
  (rcp))
