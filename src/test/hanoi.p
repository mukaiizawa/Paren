(function hanoi (n)
  (let ((hanoi-move (lambda (n a b)
                      (if (> n 1) (hanoi-move (-- n) a (- 6 a b)))
                      (print (list 'move n 'from a 'to b))
                      (if (> n 1) (hanoi-move (-- n) (- 6 a b) b)))))
    (hanoi-move n 1 2)))

(hanoi 3)
