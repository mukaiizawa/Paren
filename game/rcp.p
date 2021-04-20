; Rock paper scissors.

(import :random)

(<- $choices '(rock paper scissors))

(function choice->index (c)
  (find (f (x) (if (memprefix? (car x) c) (cadr x)))
        (zip $choices (.. (len $choices)))))

(function user-choice ()
  (write-line "r(ock), p(aper), s(cissors):")
  (write-bytes "> ")
  (let (choice (choice->index (read-line)))
    (if choice choice
        (begin
          (write-line "illegal choice")
          (user-choice)))))

(function computer-choice ()
  (randint 3))

(function rcp ()
  (let (user-choice (user-choice) computer-choice (computer-choice))
    (write-line (str "computer: " (nth computer-choice $choices)))
    (if (= user-choice computer-choice) (write-line "draw")
        (= (% (++ user-choice) 3) computer-choice) (write-line "lose")
        (write-line "win"))
    (rcp)))

(function! main (args)
  (rcp))
