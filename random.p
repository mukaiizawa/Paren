; ramdom module.

(<- $random.seed 2463534242)

(function randomize (seed)
  (<- $random.seed seed))

(function randint (n)
  ; Returns a random integer N such that (<= 0 N (-- n)).
  (Math.truncate (* (random) n)))

(function randbool ()
  (= (randint 2) 0))

(function random ()
  ; Returns the next random floating point number in the range [0.0, 1.0).
  (/ (<- $random.seed (^ $random.seed (<< $random.seed 13))
         $random.seed (^ $random.seed (>> $random.seed 17))
         $random.seed (& (^ $random.seed (<< $random.seed 5)) 0xffffffff))
     0x100000000))
