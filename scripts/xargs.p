; xargs.

(import :optparse)

(function run (cmd partial-args rest-args)
  (let (args (join (cons cmd (concat partial-args (->list rest-args))) " "))
    (if (! $only-show?) (system args)
        (write-line args))))

(function xargs (cmd partial-args)
  (if (! $combine?) (foreach (partial run cmd partial-args) (collect read-line))
      (run cmd partial-args (collect read-line))))

(function! main (args)
  (let ((op (cmd :rest partial-args)) (.parse (.init (.new OptionParser) "cs") args))
    (<- $combine? (.get op "c")
        $only-show? (.get op "s"))
    (xargs cmd partial-args)))
