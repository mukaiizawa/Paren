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
    (println "Your double swing has a strength of " x)
    (.hit (.pick-monster self) x)
    (if (! (monsters-dead?)) (.hit (.pick-monster self) x))))

(method Player .round-house ()
  (dotimes (x (++ (rand1-n (// self->strength 3))))
    (if (monsters-dead?) (return nil)
        (.hit (.pick-monster-randomly self) 1))))

(method Player .attack ()
  (println)
  (print "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (let (cmd (read))
    (if (== cmd 's) (.stab self)
        (== cmd 'd) (.double-swing self)
        (== cmd 'r) (.round-house self)
        (begin
          (println "That is not a valid attack style.")
          (.attack self)))))

(method Player .hit (x :key type)
  (if (== type :health) (<- self->health (- self->health x))
      (== type :agility) (<- self->agility (- self->agility x))
      (== type :strength) (<- self->strength (- self->strength x))
      (assert nil)))

(method Player .pick-monster ()
  (println)
  (print "Monster #:")
  (let (x (read) m nil)
    (if (! (&& (int? x) (<= 0 x (-- $monster-count)))) (println "That is not a valid monster number.")
        (.dead? (<- m ([] $monsters x))) (println "That monster is alread dead.")
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

(method Monster .name ()
  (.symbol (.class self)))

(method Monster .hit (x)
  (<- self->health (- self->health x))
  (if (.dead? self) (println "You killed the " (.name self) "!")
      (println "You hit the " (.name self) ", knocking off " x  " health points!")))

(method Monster .attack ()
  (raise NotImplementedError))

(method Monster .describe ()
  (str "A fierce " (.name self)))

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
    (println "An orc swings his club at you and knocks off " x " of your health points.")
    (.hit $player x :type :health)))

(method Orc .describe ()
  (str "A wicked orc with a level " self->club-level " club"))

(define-monster Hydra ())

(method Hydra .attack ()
  (let (x (rand1-n (>> (.health self) 1)))
    (println "A hydra attacks you with " x " of its heads! It also grows back one more head!")
    (<- self->health (++ self->health))
    (.hit $player x :type :health)))

(method Hydra .hit (x)
  (<- self->health (- self->health x))
  (if (.dead? self) (println "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
      (println "You lop off " x " of the hydra's heads!")))

(method Hydra .describe ()
  (str "A malicious hydra with " (.health self) " heads."))

(define-monster Slime () sliminess)

(method Slime .init ()
  (Monster.init self)
  (<- self->sliminess (rand1-n 5))
  self)

(method Slime .attack ()
  (let (x (rand1-n self->sliminess))
    (println (str "A slime mold wraps around your legs and decreases your agility by " x "!"))
    (.hit $player x :type :agility)
    (when (rand.bool)
      (println "It also squirts in your face, taking away a health point!")
      (.hit $player 1 :type :health))))

(method Slime .describe ()
  (str "A slime mold with a sliminess of " self->sliminess))

(define-monster Brigand ())

(method Brigand .attack ()
  (let (x (max (.health $player) (.agility $player) (.strength $player)))
    (if (= x (.health $player))
        (begin
          (println "A brigand hits you with his slingshot, taking off 2 health points!")
          (.hit $player 2 :type :health))
        (= x (.agility $player))
        (begin
          (println "A brigand catches your leg with his whip, taking off 2 agility points!")
          (.hit $player 2 :type :agility))
        (= x (.strength $player))
        (begin
          (println "A brigand cuts your arm with his whip, taking off 2 strength points!")
          (.hit $player 2 :type :strength))
        (assert nil))))

; game

(function monsters-dead? ()
  (doarray (m $monsters)
    (if (! (.dead? m)) (return nil)))
  true)

(function show-monsters ()
  (println)
  (println "Your foes:")
  (dotimes (i $monster-count)
    (let (m ([] $monsters i))
      (println i ". " (.to-s m)))))

(function game-loop ()
  (if (|| (.dead? $player) (monsters-dead?)) (return nil))
  (println)
  (println (.to-s $player))
  (dotimes (k (.action-point $player))
    (show-monsters)
    (.attack $player)
    (if (monsters-dead?) (return nil)))
  (println)
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
  (if (.dead? $player) (println "You have been killed. Game Over.")
      (println "Congratulations! You have vanquished all of your foes.")))
