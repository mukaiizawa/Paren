; Conrad Barski's Land of Lisp orc-battle implementation.

(import :rand)

(<- $player nil
    $monster-classes nil
    $monster-count 12
    $monsters (array $monster-count))

(function rand1-n (n)
  (++ (rand.int (max n 1))))

; Player

(class Player ()
  health agility strength)

(method Player .init ()
  (<- self->health 30
      self->agility 30
      self->strength 30)
  self)

(method Player .health () self->health)
(method Player .agility () self->agility)
(method Player .strength () self->strength)

(method Player .to-s ()
  (str "You are a valiant knight with a health of " self->health
       ", an agility of " self->agility
       ", and a strength of " self->strength))

(method Player .action-point ()
  (++ (// self->agility 15)))

(method Player .stab ()
  (.hit (.pick-monster self) (+ 2 (rand1-n (>> self->strength 1)))))

(method Player .double-swing ()
  (let (x (rand1-n (// self->strength 6)))
    (write-line (str "Your double swing has a strength of " x))
    (.hit (.pick-monster self) x)
    (if (! (monsters-dead?)) (.hit (.pick-monster self) x))))

(method Player .round-house ()
  (dotimes (x (++ (rand1-n (// self->strength 3))))
    (if (monsters-dead?) (return nil)
        (.hit (.pick-monster-randomly self) 1))))

(method Player .attack ()
  (write-line)
  (write-bytes "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (let (cmd (read))
    (if (= cmd 's) (.stab self)
        (= cmd 'd) (.double-swing self)
        (= cmd 'r) (.round-house self)
        (begin
          (write-line "That is not a valid attack style.")
          (.attack self)))))

(method Player .hit (x :key type)
  (if (= type :health) (<- self->health (- self->health x))
      (= type :agility) (<- self->agility (- self->agility x))
      (= type :strength) (<- self->strength (- self->strength x))
      (assert nil)))

(method Player .pick-monster ()
  (write-line)
  (write-bytes "Monster #:")
  (let (x (read) m nil)
    (if (! (&& (int? x) (<= 0 x (-- $monster-count)))) (write-line "That is not a valid monster number.")
        (.dead? (<- m ([] $monsters x))) (write-line "That monster is alread dead.")
        (return m))
    (.pick-monster self)))

(method Player .pick-monster-randomly ()
  (let (m ([] $monsters (rand.int $monster-count)))
    (if (.dead? m) (.pick-monster-randomly self)
        m)))

(method Player .dead? ()
  (<= self->health 0))

; Monster

(macro define-monster (name (:opt super :rest features) :rest fields)
  `(begin (class ,name ,(if super (cons super features) '(Monster))
            ,@fields)
          (push! ,name $monster-classes)))

(class Monster () health)

(method Monster .init ()
  (<- self->health (rand1-n 10))
  self)

(method Monster .health ()
  self->health))

(method Monster .dead? ()
  (<= self->health 0))

(method Monster .hit (x)
  (<- self->health (- self->health x))
  (write-line
    (if (.dead? self) (str "You killed the " (.symbol (.class self)) "!")
        (str "You hit the " (.symbol (.class self)) ", knocking off " x  " health points!"))))

(method Monster .attack ()
  (assert nil))

(method Monster .describe ()
  (str "A fierce " (.symbol (.class self))))

(method Monster .to-s ()
  (if (.dead? self) "**dead**"
      (str "(Health=" self->health ") " (.describe self))))

(define-monster Orc () club-level)

(method Orc .init ()
  (Monster.init self)
  (<- self->club-level (rand1-n 8))
  self)

(method Orc .attack ()
  (let (x (rand1-n self->club-level))
    (write-line (str "An orc swings his club at you and knocks off " x " of your health points."))
    (.hit $player x :type :health)))

(method Orc .describe ()
  (str "A wicked orc with a level " self->club-level " club"))

(define-monster Hydra ())

(method Hydra .attack ()
  (let (x (rand1-n (>> (.health self) 1)))
    (write-line (str "A hydra attacks you with " x " of its heads! It also grows back one more head!"))
    (<- self->health (++ self->health))
    (.hit $player x :type :health)))

(method Hydra .hit (x)
  (<- self->health (- self->health x))
  (write-line
    (if (.dead? self) "The corpse of the fully decapitated and decapacitated hydra falls to the floor!"
        (str "You lop off " x " of the hydra's heads!"))))

(method Hydra .describe ()
  (str "A malicious hydra with " (.health self) " heads."))

(define-monster Slime () sliminess)

(method Slime .init ()
  (Monster.init self)
  (<- self->sliminess (rand1-n 5))
  self)

(method Slime .attack ()
  (let (x (rand1-n self->sliminess))
    (write-line (str "A slime mold wraps around your legs and decreases your agility by " x "!"))
    (.hit $player x :type :agility)
    (when (rand.bool)
      (write-line "It also squirts in your face, taking away a health point!")
      (.hit $player 1 :type :health))))

(method Slime .describe ()
  (str "A slime mold with a sliminess of " self->sliminess))

(define-monster Brigand ())

(method Brigand .attack ()
  (let (x (max (.health $player) (.agility $player) (.strength $player)))
    (if (= x (.health $player))
        (begin
          (write-line "A brigand hits you with his slingshot, taking off 2 health points!")
          (.hit $player 2 :type :health))
        (= x (.agility $player))
        (begin
          (write-line "A brigand catches your leg with his whip, taking off 2 agility points!")
          (.hit $player 2 :type :agility))
        (= x (.strength $player))
        (begin
          (write-line "A brigand cuts your arm with his whip, taking off 2 strength points!")
          (.hit $player 2 :type :strength))
        (assert nil))))

; game

(function monsters-dead? ()
  (doarray (m $monsters)
    (if (! (.dead? m)) (return nil)))
  true)

(function show-monsters ()
  (write-line)
  (write-line "Your foes:")
  (dotimes (i $monster-count)
    (let (m ([] $monsters i))
      (write-line (str i ". " (.to-s m))))))

(function game-loop ()
  (if (|| (.dead? $player) (monsters-dead?)) (return nil))
  (write-line)
  (write-line (.to-s $player))
  (dotimes (k (.action-point $player))
    (show-monsters)
    (.attack $player)
    (if (monsters-dead?) (return nil)))
  (write-line)
  (doarray (m $monsters)
    (if (! (.dead? m)) (.attack m)))
  (game-loop))

(function init-game ()
  (<- $player (.new Player))
  (dotimes (i $monster-count)
    ([] $monsters i (.new ([] $monster-classes (rand.int (len $monster-classes)))))))

(function! main (args)
  (init-game)
  (game-loop)
  (if (.dead? $player) (write-line "You have been killed. Game Over.")
      (write-line "Congratulations! You have vanquished all of your foes.")))
