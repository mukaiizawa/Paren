; print a sequence of numbers.

(import :optparse)

(function seq (first inc last :key width)
  ; seq [OPTION] [[FIRST [INCREMENT]] LAST]
  ; Print numbers from FIRST to LAST, in steps of INCREMENT.
  ; If FIRST or INCREMENT is omitted, it defaults to 1. 
  ;     -w equalize width by padding with leading zeroes
  (let (fmt (str "%0" width "d"))
    (foreach write-line 
             (map (f (x) (format fmt x))
                  (.. first (++ last) inc)))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "w:") args)
                  (arg1 :opt arg2 arg3) (map int args)
                  width (int (.get op "w")))
    (if arg3 (seq arg1 arg2 arg3 :width width)
        arg2 (seq arg1 1 arg2 :width width)
        (seq 1 1 arg1 :width width))))
