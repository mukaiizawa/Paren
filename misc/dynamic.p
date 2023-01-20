; lexical vs dynamic scope.

(<- xxx :global
    fx (f () (dynamic xxx))
    gx (f (x) (dynamic xxx))
    hx (f (x) (gx x)))

(if (! (== xxx :global)) (throw (list 1 xxx)))
(if (! (== (let (xxx :local) xxx) :local)) (throw (list 2 xxx)))
(if (! (== (let (xxx :local) (dynamic xxx)) :local)) (throw (list 3 xxx)))
(if (! (== (fx) :global)) (throw (list 4 xxx)))
(if (! (== (let (xxx :local) (fx)) :local)) (throw (list 5 xxx)))
(if (! (== (let (xxx :local) (let (deep :depth) (fx))) :local)) (throw (list 6 xxx)))
(if (! (== (gx :dummy) :global)) (throw (list 7 xxx)))
(if (! (== (let (xxx :local) (gx :dummy)) :local)) (throw (list 8 xxx)))
(if (! (== (let (xxx :local) (let (deep :depth) (gx :dummy))) :local)) (throw (list 9 xxx)))

(throw :pass)
