; Rock paper scissors.

(import :rand)

(<- $choices '("rock" "paper" "scissors"))

(function choice->index (c)
  (position (f (x) (prefix? x c)) $choices))

(function user-choice ()
  (write-line "r(ock), p(aper), s(cissors):")
  (write-bytes "> ")
  (let (choice (choice->index (read-line)))
    (if choice choice
        (begin
          (write-line "illegal choice")
          (user-choice)))))

(function computer-choice ()
  (rand.int 3))

(function rcp ()
  (let (user-choice (user-choice) computer-choice (computer-choice))
    (write-line (str "computer: " ([] $choices computer-choice)))
    (if (= user-choice computer-choice) (write-line "draw")
        (= (% (++ user-choice) 3) computer-choice) (write-line "lose")
        (write-line "win"))
    (rcp)))

(function! main (args)
  (rcp))