; tower of hanoi.

(function move (n from to via)
  (when (> n 0)
    (move (-- n) from via to)
    (write (list :from from :to to))
    (move (-- n) via to from)))

(function main (args)
  (move 3 'A 'B 'C))
