; Levenshtein distance.

(function levenshtein-distance (s t)
  (if (empty? s) (len t)
      (empty? t) (len s)
      (= ([] s 0) ([] t 0)) (levenshtein-distance (slice s 1) (slice t 1))
      (++ (min (levenshtein-distance s (slice t 1))
               (levenshtein-distance (slice s 1) t)
               (levenshtein-distance (slice s 1) (slice t 1))))))

(function! main (args)
  (assert (= (levenshtein-distance "kitten" "sitting") 3))
  (assert (= (levenshtein-distance "vinter" "writers") 4)))
