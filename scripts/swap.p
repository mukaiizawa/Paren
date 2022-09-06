; swap files.

(import :csv)
(import :optparse)
(import :tempfile)

(function move (src dst)
  (let ((src dst) (map path (list src dst)))
    (if (nil? $silent?) (write (list :src (.to-s src) :dst (.to-s dst))))
    (if (nil? $dry-run?) (.rename src dst))))

(function representative-node (node rd)
  ; (a b) => a
  ; (x y) (y z) (z x) => x
  ; (a1 b1) (b1 c1) (c1 d1) => a1
  (let (parent nil found nil)
    (loop
      (when (in? node found)
        (return (car (sort! found))))    ; circular linked list.
      (push! node found)
      (if (<- parent ([] rd node)) (<- node parent)
          (return node)))))

(function representative-nodes (fd rd)
  (uniq
    (sort!
      (map (f (x) (representative-node x rd))
           (keys fd)))))

(function swap-files (edges)
  (let (wk nil)
    (unwind-protect
      (let (fd (dict) rd (dict))
        (dolist (edge edges)
          (let ((src dst) edge)
            (if (nil? (.file? (path src))) (raise ArgumentError (format "missing file `%s`" src))
                (in? src fd) (raise ArgumentError (format "duplicate src file `%s`" src))
                (in? dst rd) (raise ArgumentError (format "duplicate dst file `%s`" dst))
                (begin
                  ([] fd src dst)
                  ([] rd dst src)))))
        (<- wk (.tempdir (path "./")))
        (dolist (rep-node (representative-nodes fd rd))
          (let (tail ([] rd rep-node))
            (when (nil? tail)
              (<- tail ([] fd rep-node))
              (while ([] fd tail) (<- tail ([] fd tail))))
            (move tail (.resolve wk tail))
            (let (dst tail src ([] rd tail))
              (loop 
                (move src dst)
                (if (= src rep-node) (break)
                    (<- dst src
                        src ([] rd dst)))))
            (move (.resolve wk tail) rep-node))))
      (if wk (.remove wk)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "nf:s") args))
    (<- $dry-run? (.get op "n")
        $silent? (.get op "s"))
    (if (nil? (.get op "f")) (swap-files (group args 2))
        (with-open ($in (.get op "f") :read)
          (swap-files (collect (partial .read (.new CSVReader))))))))
