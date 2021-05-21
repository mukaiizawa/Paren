; what is the progress.

(import :rand)

(function! main (args)
  (let (lis0 '("進捗" "どう" "です" "か?")
             rand-word (f () (nth (rand.int (len lis0)) lis0))
             rec (f (next lis)
                   (when lis
                     (write-bytes next)
                     (if (= next (car lis)) (rec (rand-word) (cdr lis))
                         (rec (rand-word)lis0)))))
    (rec (rand-word) lis0)))
