; Pig Latin.

(<- $vowel (split "aiueo"))

(function vowel-pos (chars)
  (position (f (ch) (in? ch $vowel))
            chars))

(function split-alpha (word)
  (let (wlen (len word) prefix-end 0 suffix-start wlen)
    (dotimes (i wlen)
      (if (alpha? ([] word (<- prefix-end i))) (break)))
    (dotimes (i wlen)
      (if (alpha? ([] word (-- (<- suffix-start (- wlen i))))) (break)))
    (if (>= prefix-end suffix-start) (list word "" "")
        (list (slice word 0 prefix-end)
              (slice word prefix-end suffix-start)
              (slice word suffix-start)))))

(function ->pig-latin1 (word)
  (let ((prefix word suffix) (split-alpha word)
                             pos (vowel-pos (map lower (split word)))
                             pig-latin (if (empty? word) word
                                           (nil? pos) (concat word "ay")
                                           (= pos 0) (concat word "yay")
                                           (concat (slice word pos) (slice word 0 pos) "ay")))
    (concat prefix
            (if (title? word) (title pig-latin)
                (upper? word) (upper pig-latin)
                pig-latin)
            suffix)))

(function ->pig-latin (words)
  (join (map ->pig-latin1 words) " "))

; For words that begin with consonant sounds, all letters before the initial vowel are placed at the end of the word sequence.
; Then, "ay" is added, as in the following examples:
(assert (= (->pig-latin1 "pig") "igpay"))
(assert (= (->pig-latin1 "latin") "atinlay"))
(assert (= (->pig-latin1 "banana") "ananabay"))
(assert (= (->pig-latin1 "will") "illway"))
(assert (= (->pig-latin1 "butler") "utlerbay"))
(assert (= (->pig-latin1 "happy") "appyhay"))
(assert (= (->pig-latin1 "duck") "uckday"))
(assert (= (->pig-latin1 "me") "emay"))
(assert (= (->pig-latin1 "bagel") "agelbay"))
(assert (= (->pig-latin1 "history") "istoryhay"))

; When words begin with consonant clusters (multiple consonants that form one sound), the whole sound is added to the end when speaking or writing.
(assert (= (->pig-latin1 "smile") "ilesmay"))
(assert (= (->pig-latin1 "string") "ingstray"))
(assert (= (->pig-latin1 "stupid") "upidstay"))
(assert (= (->pig-latin1 "glove") "oveglay"))
(assert (= (->pig-latin1 "trash") "ashtray"))
(assert (= (->pig-latin1 "floor")"oorflay"))
(assert (= (->pig-latin1 "store")"orestay"))

; For words that begin with vowel sounds, the vowel is left alone, and most commonly 'yay' is added to the end.
(assert (= (->pig-latin1 "eat") "eatyay"))
(assert (= (->pig-latin1 "omelet") "omeletyay"))
(assert (= (->pig-latin1 "are") "areyay"))
(assert (= (->pig-latin1 "egg") "eggyay"))
(assert (= (->pig-latin1 "explain") "explainyay"))
(assert (= (->pig-latin1 "always") "alwaysyay"))
(assert (= (->pig-latin1 "ends") "endsyay"))
(assert (= (->pig-latin1 "I")"Iyay"))

(function! main (args)
  (if (nil? args) (loop (write-line (->pig-latin (split (read-line) " "))))
      (write-line (->pig-latin args))))
