; sudoku solver.

(import :matrix)
(import :re)

(<- $size 9 $box-size 3
    $set (.. 1 (++ $size))
    $board (matrix (list $size $size)))

(function show ()
  (dotimes (i $size)
    (if (= (% i 3) 0) (write-line "+---+---+---+"))
    (dotimes (j $size)
      (if (= (% j 3) 0) (write-bytes "|"))
      (let (val (.at $board (list i j)))
        (write-bytes (if (nil? val) " " (str val)))))
    (write-line "|"))
  (write-line "+---+---+---+"))

(function choices (p)
  (let ((row col) p)
    (difference
      $set
      (flatten
        (list (.at $board p)
              (map (f (x)
                     (list (.at $board (list row x))
                           (.at $board (list x col))))
                   (.. $size))
              (map (f (x)
                     (map (f (y)
                            (.at $board (list (+ x (* (// row $box-size) $box-size))
                                              (+ y (* (// col $box-size) $box-size)))))
                          (.. $box-size)))
                   (.. $box-size)))))))

(function preprocess ()
  (domatrix (p $board)
    (if (nil? (.at $board p))
        (let (choices (choices p))
          (when (= (len choices) 1)
            (.put $board p (car choices))
            (return (preprocess)))))))

(function step (p)
  (let ((row col) p)
    (if (= row $size) (begin (show) (quit))
        (= col $size) (step (list (++ row) 0))
        (! (nil? (.at $board p))) (step (list row (++ col)))
        (dolist (x (choices p))
          (.put $board p x)
          (step (list row col))
          (.put $board p nil)))))

(function load-file (file)
  (with-open ($in file :read)
    (let (lines nil n 0)
      (while (< n $size)
        (let (line (read-line))
          (if (nil? line) (raise EOFError)
              (empty? line) (continue)
              (! (prefix? line "+"))
              (begin
                (push! line lines)
                (<- n (++ n))))))
      (let (lines (reverse! lines) expr (re.compile "[|+\\-]"))
        (dotimes (i $size)
          (let (vals (split (re.replace expr (pop! lines) "")))
            (dotimes (j $size)
              (let (val (pop! vals))
                (if (nil? val) (raise ArgumentError (list "illegal line" vals))
                    (.put $board (list i j) (&& (! (space? val)) (int val))))))))))))

(function! main (args)
  ; sudoku-solver.p FILE
  ; Solve a partially filled-in 9x9 Sudoku grid and display the result.
  ;
  ; The file is read according to the following rules.
  ; - empty line are ignored
  ; - starting with `+` are ignored
  ; - `|` are ignored
  ;
  ;     +---+---+---+
  ;     |85 |  2|4  |
  ;     |72 |   |  9|
  ;     |  4|   |   |
  ;     +---+---+---+
  ;     |   |1 7|  2|
  ;     |3 5|   |9  |
  ;     | 4 |   |   |
  ;     +---+---+---+
  ;     |   | 8 | 7 |
  ;     | 17|   |   |
  ;     |   | 36| 4 |
  ;     +---+---+---+
  ;
  (if (nil? args) (raise ArgumentError)
      (load-file (car args)))
  (show)
  (preprocess)
  (show)
  (step '(0 0)))
