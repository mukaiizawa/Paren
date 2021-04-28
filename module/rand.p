; random module.

(<- $rand.seed 2463534242)

(function rand.seed (seed)
  ; Initialize the random number generator with seed.
  (<- $rand.seed seed))

(function rand.int (n)
  ; Returns a random integer N such that (<= 0 N (-- n)).
  (// (* (rand.val) n)))

(function rand.bool ()
  ; Randomly returns a boolean value.
  (= (rand.int 2) 0))

(function rand.bytes (bytes)
  ; Returns the specified bytes with a random value.
  (dotimes (i (len bytes))
    ([] bytes i (rand.int 0xff)))
  bytes)

(function rand.str (size :key alnum? alpha? numeric? lower? upper?)
  ; Returns alphanumeric string.
  (let (numeric (if (|| alnum? numeric?) "0123456789")
                lower (if (|| alnum? alpha? lower?) "abcdefghijklmnopqrstuvwxyz")
                upper (if (|| alnum? alpha? upper?) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                table (str numeric lower upper)
                table-size (len table))
    (assert (> table-size 0))
    (with-memory-stream ($out)
      (dotimes (i size) (write-bytes ([] table (rand.int table-size)))))))

(function rand.val ()
  ; Returns the next random floating point number in the range [0.0, 1.0).
  (/ (<- $rand.seed (^ $rand.seed (<< $rand.seed 13))
         $rand.seed (^ $rand.seed (>> $rand.seed 17))
         $rand.seed (& (^ $rand.seed (<< $rand.seed 5)) 0xffffffff))
     0x100000000))
