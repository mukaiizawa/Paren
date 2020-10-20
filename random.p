; random module.

(<- $random.seed 2463534242)

(function randomize (seed)
  ; Initialize the random number generator with seed.
  (<- $random.seed seed))

(function randint (n)
  ; Returns a random integer N such that (<= 0 N (-- n)).
  (// (* (random) n)))

(function randbool ()
  ; Randomly returns a boolean value.
  (= (randint 2) 0))

(function randbytes (bytes)
  ; Returns the specified bytes with a random value.
  (dotimes (i (memlen bytes))
    ([] bytes i (randint 0xff)))
  bytes)

(function randstr (size :key alnum? alpha? numeric? lower? upper?)
  ; Returns alphanumeric string.
  (let (numeric (if (|| alnum? numeric?) "0123456789")
                lower (if (|| alnum? alpha? lower?) "abcdefghijklmnopqrstuvwxyz")
                upper (if (|| alnum? alpha? upper?) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                table (string numeric lower upper)
                table-size (memlen table)
                val (bytes size))
    (assert (> table-size 0))
    (dotimes (i size)
      ([] val i ([] table (randint table-size))))
    (mem->str! val)))

(function random ()
  ; Returns the next random floating point number in the range [0.0, 1.0).
  (/ (<- $random.seed (^ $random.seed (<< $random.seed 13))
         $random.seed (^ $random.seed (>> $random.seed 17))
         $random.seed (& (^ $random.seed (<< $random.seed 5)) 0xffffffff))
     0x100000000))
