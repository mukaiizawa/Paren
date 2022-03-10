; what is the progress.

(import :rand)

(function! main (args)
  (let (lis0 '("進捗" "どう" "です" "か?")
             rec (f (rest)
                   (when rest
                     (let (next (rand.choice lis0))
                       (write-bytes next)
                       (if (= next (car rest)) (rec (cdr rest))
                           (rec lis0))))))
    (rec lis0)))
