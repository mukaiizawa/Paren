; dirname.

(function! main (args)
  (foreach write-line
           (map (f (x)
                  (let (p (.parent (path x)))
                    (if (nil? p) "."
                        (.to-s p))))
                (collect read-line))))
