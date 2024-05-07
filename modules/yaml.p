; yaml module.

(class YAML.Reader (AheadReader))

(method YAML.Reader .parse-scalar ()
  nil)

(method YAML.Reader .parse-sequece ()
  (let (lis nil)
    (while (= (.next (.skip-space self)) "-")
      (.skip self)
      (push! (.read self) lis))))

(method YAML.Reader .read ()
  (let (next (.next (.skip-space self)))
    (if (nil? next) nil
        (= next "#") (begin (.skip-line self) (.read self))
        (= next "-") (.parse-sequece self)
        (.parse-scalar self))))

(function yaml.read ()
  (.read (.new YAML.Reader)))

(function! main (args)
  ;; comment
  (assert (nil? (with-memory-stream ($in "
                                         # foo
                                         # bar
                                         ")
                                         (yaml.read))))
  ;; sequence
  (assert (= (with-memory-stream ($in "
                                      - Mark McGwire
                                      - Sammy Sosa
                                      - Ken Griffey
                                      ")
                                      (yaml.read))
             (list "Mark McGwire"
                   "Sammy Sosa"
                   "Ken Griffey"))))
