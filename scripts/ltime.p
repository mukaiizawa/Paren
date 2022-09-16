; list time.

(import :optparse)

(function hhmm->min (hhmm default)
  (if (nil? hhmm) default
      (let ((hh mm) (map int (split hhmm ":")))
        (+ (* 60 hh) mm))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "e:d:s:") args))
    (for (s (hhmm->min (.get op "s") 0)
            e (hhmm->min (.get op "e") (* 24 60))
            dt (.get-int op "d" 60)) (<= s e) (s (+ s dt))
        (printf "%02d:%02d\n" (// s 60) (% s 60)))))
