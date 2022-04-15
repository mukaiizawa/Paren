; removeing tags.

(import :xml)

(function detag (tree)
  (foreach (f (x) (if (string? x) (write-line x) (detag x)))
           (cddr tree)))

(function! main (args)
  (let (rd (.new XMLReader))
    (foreach detag (collect (f () (.read rd))))))
