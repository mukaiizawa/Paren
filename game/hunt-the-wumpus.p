; Hunt the Wumpus.

(import :rand)

(<- $room-count 20
    $neigbor-rooms
    '((1 4 7) (0 2 9) (1 3 11) (2 4 13) (0 3 5)
              (4 6 14) (5 7 16) (0 6 8) (7 9 17) (1 8 10)
              (9 11 18) (2 10 12) (11 13 19) (3 12 14) (5 13 15)
              (14 16 19) (6 15 17) (8 16 18) (10 17 19) (12 15 18))
    $arrow-count 5
    $wumpus-chance 0.75
    $player nil
    $wumpus nil
    $bats nil
    $pits nil)

(function neighbours (room)
  ([] $neigbor-rooms room))

(function end-game (s :opt win?)
  (write-line s)
  (if (! win?) (write-line "Game Over!"))
  (quit))

(function shoot (room)
  (if (= room $wumpus)
      (end-game
        "Congratulations, you have slain the Wumpus!" :win)
      (= (<- $arrow-count (-- $arrow-count)) 0)
      (end-game
        "You have run out of arrows.")
      (begin
        (write-line "Miss!")
        (if (<= (rand.val) $wumpus-chance)
            (begin
              (write-line "The Wumpus moves to a nearby cavern.")
              (if (= $player (<- $wumpus (rand.choice (neighbours $wumpus))))
                  (end-game
                    "The Wumpus attacked you! You've been killed.")))))))

(function random-move ()
  (let (next-room (rand.int $room-count))
    (if (in? next-room (concat (list $player $wumpus) $bats $pits)) (random-move)
        (<- $player next-room))))

(function move (next-room)
  (if (= (<- $player next-room) $wumpus)
      (end-game
        "The Wumpus has eaten you!")
      (in? $player $bats) 
      (write-line
        (str "Argh! A Giant Bat has carried you to room " (random-move) "."))
      (in? $player $pits)
      (end-game
        "You have fallen down a pit!")))

(function input ()
  (catch (Error (f (e) (write-line (.to-s e)) (input)))
    (write-bytes "> ")
    (let ((cmd room) (split (read-line) " "))
      (if (! (in? cmd '("w" "s"))) (raise Error "Unknown command")
          (! (in? (<- room (int room)) (neighbours $player))) (raise Error "You cannot move or shoot there!")
          (list (symbol cmd) room)))))

(function game-loop ()
  (let (neighbours (neighbours $player))
    (write (list :room $player :neigbor-rooms neighbours :arrow-count $arrow-count))
    (write-line "You can (w)alk or (s)hoot to rooms ")
    (if (in? $wumpus neighbours) (write-line "You smell something nearby."))
    (if (intersection $bats neighbours) (write-line "You hear a rustling."))
    (if (intersection $pits neighbours) (write-line "You feel a cold wind blowing from a nearby cavern."))
    (let ((cmd room) (input))
      (if (== cmd 'w) (move room)
          (== cmd 's) (shoot room)
          (assert nil))
      (game-loop))))

(function! main (args)
  (let (rooms (rand.shuffle! (.. 1 (++ $room-count))))
    (<- $player 0
        $wumpus ([] rooms 0)
        $bats (list ([] rooms 1) ([] rooms 2))
        $pits (list ([] rooms 3) ([] rooms 4)))
    (game-loop)))
