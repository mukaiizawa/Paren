; Rock paper scissors Object-oriented version.

(import :rand)
(import :rsp "./")

(class Player () name)

(method Player .to-s ()
  self->name)

(method Player .choice ()
  (raise NotImplementedError))

(method Player .show (choice)
  (println (.to-s self) ": choose " ([] $choices choice))
  choice)

(class Man (Player))

(method Man .init (name)
  (<- self->name name)
  self)

(method Man .choice ()
  (print (.to-s self) ": r(ock), p(aper), s(cissors) >> ")
  (let (choice (choice->index (read-line)))
    (if (nil? choice)
        (begin
          (println "illegal choice")
          (.choice self))
        (.show self choice))))

(class Computer (Player))

(method Computer .init ()
  (<- self->name  "Computer")
  self)

(method Computer .choice ()
  (.show self (rand.int 3)))

(class RockComputer (Computer))

(method RockComputer .choice ()
  (.show self 0))

(function choice-player (message)
  (println message)
  (print "0:user, 1:Computer, 2:Computer(uses only Rock)>> ")
  (let (line (read-line))
    (if (= line "0") (begin
                       (print "your name>> ")
                       (.init (.new Man) (read-line)))
        (= line "1") (.new Computer)
        (= line "2") (.new RockComputer)
        (begin
          (println "illegal choice")
          (choice-player message)))))

(function! rsp (p1 p2)
  (let (choice1 (.choice p1) choice2 (.choice p2))
    (if (= choice1 choice2) (println "draw")
        (= (% (++ choice1) 3) choice2) (println "Player2 win")
        (println "Player1 win"))
    (println "press enter") (read-line)
    (rsp p1 p2)))

(function! main (args)
  (rsp (choice-player "Player1")
       (choice-player "Player2")))
