; Sierpinski triangle.

(function init-table (table n)
  (dotimes (i (len table))
    ([] table i (if (= i n) 1 0)))
  table)

(function show-table (table)
  (doarray (x table)
    (print (if (= x 0) " " "*")))
  (println))

(function next-generation (table0 table1)
  (let (size (len table0))
    (dotimes (i size)
      ([] table1 i (^ ([] table0 (% (-- (+ i size)) size))
                      ([] table0 (% (++ i) size)))))
    (list table1 table0)))    ; switch

(function sierpinski-triangle (n)
  ; Based on Cellular automaton Rule 90.
  (let (size (++ (* 2 n)) table (init-table (array size) n) work-table (array size))
    (dotimes (i n)
      (show-table table)
      (<- (table work-table) (next-generation table work-table)))))

(function! main (args)
  (sierpinski-triangle 30))
