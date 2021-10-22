; Rock paper scissors Object-oriented version.

(import :rand)
(import :rcp "./")

(class Player () name)

(method Player .to-s ()
  (&name self))

(method Player .choice ()
  (raise NotImplementedError))

(method Player .show (choice)
  (write-line (str (.to-s self) ": choose " ([] $choices choice)))
  choice)

(class User (Player))

(method User .init (name)
  (&name! self name))

(method User .choice ()
  (write-bytes (str (.to-s self) ": r(ock), p(aper), s(cissors) >> "))
  (let (choice (choice->index (read-line)))
    (if (nil? choice)
        (begin
          (write-line "illegal choice")
          (.choice self))
        (.show self choice))))

(class Computer (Player))

(method Computer .init ()
  (&name! self "Computer"))

(method Computer .choice ()
  (.show self (rand.int 3)))

(class RockComputer (Computer))

(method RockComputer .choice ()
  (.show self 0))

(function choice-player (msg)
  (write-line msg)
  (write-bytes "0:user, 1:Computer, 2:Computer(uses only Rock)>> ")
  (let (line (read-line))
    (if (= line "0") (begin
                       (write-bytes "your name>> ")
                       (.init (.new User) (read-line)))
        (= line "1") (.new Computer)
        (= line "2") (.new RockComputer)
        (begin
          (write-line "illegal choice")
          (choice-player msg)))))

(function! rcp (p1 p2)
  (let (choice1 (.choice p1) choice2 (.choice p2))
    (if (= choice1 choice2) (write-line "draw")
        (= (% (++ choice1) 3) choice2) (write-line "Player2 win")
        (write-line "Player1 win"))
    (write-line "press enter") (read-line)
    (rcp p1 p2)))

(function! main (args)
  (rcp (choice-player "Player1")
       (choice-player "Player2")))
