; list time.

(import :optparse)

(function hhmm->min (hhmm)
  (if (nil? hhmm) nil
      (let ((hh mm) (map int (split hhmm ":")))
        (+ (* 60 hh) mm))))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "e:d:s:") args))
    (let (m (|| (hhmm->min (.get op "s")) 0)
            e (|| (hhmm->min (.get op "e")) (* 24 60))
            dm (|| (.get-int op "d") 60))
      (while (<= m e)
        (write-line (format "%02d:%02d" (// m 60) (% m 60)))
        (<- m (+ m dm))))))
