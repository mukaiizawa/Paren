; Gregory Yob's Hunt the Wumpus.

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
    $pits nil
    $instructions 
"
WELCOME TO 'HUNT THE WUMPUS'
THE WUMPUS LIVES IN A CAVE OF 20 ROOMS.
EACH ROOM HAS 3 TUNNELS LEADING TO OTHER ROOMS.
(LOOK AT A DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW WHAT A DODECAHEDRON IS, ASK SOMEONE)

HAZARDS:
BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM IF YOU GO THERE, YOU FALL INTO THE PIT (& LOSE!)
SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU GO THERE, A BAT GRABS YOU AND TAKES YOU TO SOME OTHER ROOM AT RANDOM. (WHICH MAY BE TROUBLESOME)

WUMPUS:
THE WUMPUS IS NOT BOTHERED BY HAZARDS (HE HAS SUCKER FEET AND IS TOO BIG FOR A BAT TO LIFT).
USUALLY HE IS ASLEEP.
TWO THINGS WAKE HIM UP:
YOU SHOOTING AN ARROW OR YOU ENTERING HIS ROOM.
IF THE WUMPUS WAKES HE MOVES (P=.75) ONE ROOM OR STAYS STILL (P=.25).
AFTER THAT, IF HE IS WHERE YOU ARE, HE EATS YOU UP AND YOU LOSE!

YOU:
EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW
MOVING: YOU CAN MOVE ONE ROOM (THRU ONE TUNNEL)
ARROWS: YOU HAVE 5 ARROWS.
YOU LOSE WHEN YOU RUN OUT EACH ARROW CAN GO FROM 1 TO 5 ROOMS.
YOU AIM BY TELLING THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO.
IF THE ARROW CAN'T GO THAT WAY (IF NO TUNNEL) IT MOVES
AT RANDOM TO THE NEXT ROOM.
IF THE ARROW HITS THE WUMPUS, YOU WIN.
IF THE ARROW HITS YOU, YOU LOSE.
 
WARNINGS:
WHEN YOU ARE ONE ROOM AWAY FROM A WUMPUS OR HAZARD,
THE COMPUTER SAYS:
WUMPUS:  'I SMELL A WUMPUS'
BAT   :  'BATS NEARBY'
PIT   :  'I FEEL A DRAFT'
")

; Numbering of the caves in Hunt the Wumpus
; The caves are in complete darkness, so the player cannot see into adjacent caves
; ; instead, upon moving to a new empty cave, the game describes if they can smell a Wumpus, hear a bat, or feel a draft from a pit in one of the connected caves.
; Entering a cave with a pit ends the game due to the player falling in, while entering the cave with the Wumpus startles it
; ; the Wumpus will either move to another cave or remain and kill the player.
; If the player chooses to fire an arrow, they first select how many caves, up to five, that the arrow will travel through, and then enters each cave that the arrow moves through.
; If the player enters a cave number that is not connected to where the arrow is, the game picks a valid option at random.
; If the arrow hits the player while it is travelling, the player loses; if it hits the Wumpus, they win.
; If the arrow does not hit anything, then the Wumpus is startled and may move to a new cave; unlike the player, the Wumpus is not affected by super bats or pits.
; If the Wumpus moves to the player's location, they lose.[1]

(function neighbours (room)
  ([] $neigbor-rooms room))

(function input-int (s choices)
  (int (input s (map str choices))))

(function input (s choices)
  (catch (Error (f (e)
                  (output "NOT POSSIBLE")
                  (input s choices)))
    (write-bytes s)
    (let (text (read-line))
      (if (in? text choices) text
          (raise Error)))))

(function output (s :opt end?)
  (write-line s)
  (when end?
    (write-line (if (== end? :win) "HEE HEE HEE - THE WUMPUS'LL GET YOU NEXT TIME!!" "HA HA HA - YOU LOSE!"))
    (quit)))

(function shoot ()
  (let (pos $player n (input-int "NO. OF ROOMS (1-5)" (.. 1 (++ 5))) dirs nil)
    (dotimes (i n)
      (push! (input-int "ROOM >" (.. $room-count)) dirs))
    (dotimes (next-pos n)
      (let (neighbours (neighbours pos))
        (<- pos (if (in? next-pos neighbours) next-pos (rand.choice neighbours)))
        (if (= pos $wumpus) (output "AHA! YOU GOT THE WUMPUS!" :win)
            (= pos $player) (output "OUCH! ARROW GOT YOU!" :lose))))
    (output "MISSED")
    (if (= (<- $arrow-count (-- $arrow-count)) 0)
        (output "YOU HAVE RUN OUT OF ARROWS." :lose))
    (move-wumpus)))

(function move-wumpus ()
  (if (<= (rand.val) $wumpus-chance) (<- $wumpus (rand.choice (neighbours $wumpus))))
  (if (= $player $wumpus) (output "TSK TSK TSK- WUMPUS GOT YOU!" :lose)))

(function move (next-room)
  (if (= (<- $player next-room) $wumpus)
      (begin
        (output "...OOPS! BUMPED A WUMPUS!")
        (move-wumpus))
      (in? $player $bats)
      (begin
        (output "ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!")
        (move (rand.int $room-count)))
      (in? $player $pits)
      (begin
        (output "YYYIIIIEEEE . . . FELL IN PIT" :lose))))

(function game-loop ()
  (let (neighbours (neighbours $player))
    (output (str "YOU ARE IN ROOM " $player))
    (output (str "TUNNELS LEAD TO " neighbours))
    (if (in? $wumpus neighbours) (output "I SMELL A WUMPUS."))
    (if (intersection $bats neighbours) (output "BATS NEARBY."))
    (if (intersection $pits neighbours) (output "I FEEL A DRAFT."))
    (let (cmd (input "SHOOT OR MOVE (s-m) >" '("s" "m")))
      (if (= cmd "m") (move (input-int "WHERE TO >" neighbours))
          (= cmd "s") (shoot)
          (assert nil))
      (game-loop))))

(function! main (args)
  (let (rooms (rand.shuffle! (.. 1 (++ $room-count))))
    (<- $player 0
        $wumpus ([] rooms 0)
        $bats (list ([] rooms 1) ([] rooms 2))
        $pits (list ([] rooms 3) ([] rooms 4)))
    (output $instructions)
    (game-loop)))