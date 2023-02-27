; random password generator.

(import :optparse)
(import :rand)

(<- $digit "0123456789"
    $lower "abcdefghijklmnopqrstuvwxyz"
    $upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    $symbol "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "aAl:ns") args)
                  only-lower? (.get op "a")
                  only-upper? (.get op "A")
                  only-digit? (.get op "n")
                  only-symbol? (.get op "s")
                  chars (split
                          (if (&& (nil? only-lower?)
                                  (nil? only-upper?)
                                  (nil? only-digit?)
                                  (nil? only-symbol?)) (str $digit $lower $upper $symbol)
                              (str (&& only-lower? $lower)
                                   (&& only-upper? $upper)
                                   (&& only-digit? $digit)
                                   (&& only-symbol? $symbol)))))
    (rand.seed (time))
    (dotimes (i (.get-int op "l" 8))
      (print (rand.choice chars))
      (<- chars (rand.shuffle! chars)))
    (println)))
