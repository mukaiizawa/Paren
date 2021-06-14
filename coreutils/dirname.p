; dirname - strip non-directory suffix from file name.

(function dirname ()
  ; dirname
  ; Write standard input to standard output, with its trailing /component removed.
  (foreach write-line
           (map (f (x)
                  (let (p (.parent (path x)))
                    (if (nil? p) "."
                        (.to-s p))))
                (collect read-line))))

(function! main (args)
  (dirname))
