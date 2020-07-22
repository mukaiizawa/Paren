; ramdom module.

(global-symbol $random.seed 2463534242)

(function Random.init (seed)
  (<- $random.seed seed))

(function Random.seed ()
  $random.seed)

(function Random.val ()
  ; Returns the next random floating point number in the range [0.0, 1.0).
  (/ (Random.next) 0x100000000))

(function Random.int (n)
  ; Returns a random integer N such that (<= 0 N (-- n)).
  (Math.truncate (* (Random.val) n)))

(function Random.bool ()
  (= (Random.int 2) 0))

(function Random.next ()
  (<- $random.seed (^ $random.seed (<< $random.seed 13))
      $random.seed (^ $random.seed (>> $random.seed 17))
      $random.seed (& (^ $random.seed (<< $random.seed 5)) 0xffffffff)))
