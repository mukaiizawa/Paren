; seq.

(import :optparse)

(function seq (first inc last :key width)
  (let (fmt (str "%0" width "d\n"))
    (foreach (partial printf fmt) 
             (.. first (++ last) inc))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "w:") args)
                  (arg1 :opt arg2 arg3) (map int args)
                  width (.get-int op "w" nil))
    (if arg3 (seq arg1 arg2 arg3 :width width)
        arg2 (seq arg1 1 arg2 :width width)
        (seq 1 1 arg1 :width width))))
