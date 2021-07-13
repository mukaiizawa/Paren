; kaleid scope.

(import :console)
(import :rand)

(<- $width 80 $height 20
    $width/2 (// $width 2) $height/2 (// $height 2))

(function draw (x y ch)
  (console.cursor :x x :y y)
  (write-bytes ch))

(function! main (args)
  (loop
    (let (x (rand.int (++ $width/2)) y (rand.int (++ $height/2)) ch (rand.choice '(" " "*")))
      (draw x y ch)
      (draw (- $width x) y ch)
      (draw (- $width x) (- $height y) ch)
      (draw x (- $height y) ch))))
